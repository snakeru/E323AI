#ifndef TASKPLAN_H
#define TASKPLAN_H

#include <vector>
#include <map>
#include <stack>

#include "CE323AI.h"
#include "ARegistrar.h"

enum task{BUILD, ASSIST, ATTACK, MERGE};

class ATask: public ARegistrar {
	public:
		ATask(task _t, float3 &pos): ARegistrar(counter), t(_t), pos(&_pos) {
			counter++;
		}
		~ATask(){}

		static int counter;

		/* The task in {BUILD, ASSIST, ATTACK, MERGE} */
		task t;

		/* The group(s) involved */
		std::map<int, CGroup*> groups;

		/* The position to navigate too */
		float3 *pos;

		/* Remove this task, unreg groups involved, and make them available
		 * again 
		 */
		void remove() {
			std::map<int, CGroup*>::iterator i;
			for (i = groups.begin(); i != groups.end(); i++) {
				i->second->unreg(*this);
				i->second->busy = false;
			}
			
			std::list<ARegistrar*>::iterator j;
			for (j = records.begin(); j != records.end(); j++)
				(*j)->remove(*this);
		}

		/* Overload */
		void remove(ARegistrar &group) {
			groups.erase(group.key);
			
			if (groups.empty()) {
				std::list<ARegistrar*>::iterator i;
				for (i = records.begin(); i != records.end(); i++)
					(*i)->remove(*this);
			}
		}

		/* Add a group to this task */
		void addGroup(CGroup &group) {
			groups[group.key] = &group;
			group.reg(*this);
			group.busy = true;
			ai->pf->addGroup(group, group.pos(), pos);
		}

		/* Update this task */
		virtual void update() = 0;

		/* Reset this task for reuse */
		virtual void reset() = 0;
};

class CTaskHandler: public ARegistrar {
	public:
		CTaskHandler(AIClasses *ai): ARegistrar(500);
		~CTaskHandler(){};

		struct BuildTask: public ATask {
			BuildTask(float3 &pos, UnitType *_toBuild): 
				ATask(BUILD, pos), toBuild(_toBuild) {}

			/* The UnitType to build */
			UnitType *toBuild;

			/* Is building */
			std::map<int, bool> building;

			/* Update the build task, assumes 1 group on a task! */
			void update() {
				std::map<int, CGroup*>::iterator i;
				for (i = groups.begin(); i != groups.end(); i++) {
					CGroup *group = i->second;
					float3 dist = group->pos() - pos;
					if (!building[group->key] && dist.Length2D() <= group->range) {
						group->build(pos, toBuild);
						building[group->key] = true;
					}
				}
				/* If the buildorder is given, remove the task, unreg groups */
				if (ai->eco->hasFinishedBuilding(groups))
					remove();
			}

			/* Reset for reusal */
			void reset() {
				records.clear();
				groups.clear();
				building.clear();
			}
		};

		struct AssistTask: public ATask {
			AssistTask(BuildTask &bt): 
				ATask(ASSIST, bt.pos()), assist(&bt) { reg(bt); }

			/* The buildtask to assist */
			BuildTask *assist;

			/* Is assisting */
			std::map<int, bool> assisting;

			/* Update the assist task */
			void update() {
				std::map<int, CGroup*>::iterator i;
				for (i = groups.begin(); i != groups.end(); i++) {
					CGroup *group = i->second;
					float3 dist = group->pos() - pos;
					if (!assisting[group->key] && dist.Length2D() <= group->range) {
						group->assist(*assist);
						assisting[group->key] = true;
					}
				}
			}

			/* Reset for reusal */
			void reset() {
				records.clear();
				groups.clear();
				assisting.clear();
			}
		};

		struct AttackTask: public ATask {
			AttackTask(int _target): 
				ATask(ATTACK, &(ai->cheat->GetUnitPos(_target))), target(_target) {}

			/* The target to attack */
			int target;

			/* Is attacking */
			std::map<int, bool> attacking;

			/* Update the attack task */
			void update() {
				std::map<int, CGroup*>::iterator i;

				/* See if we can attack our target already */
				for (i = groups.begin(); i != groups.end(); i++) {
					CGroup *group = i->second;
					float3 dist = group->pos() - pos;
					if (!attacking[group->key] && dist.Length2D() <= group->range) {
						group->attack(target);
						attacking[group->key] = true;
					}
				}

				/* If the target is destroyed, remove the task, unreg groups */
				if (ai->cheat->GetUnitPos(target) == NULLVECTOR) 
					remove();
			}

			/* Reset for reusal */
			void reset() {
				records.clear();
				groups.clear();
				attacking.clear();
			}
		};

		struct MergeTask: public ATask {
			MergeTask(float3 &pos, float _range): 
				ATask(MERGE, &pos), range(_range) {}

			/* Update the merge task */
			void update() {
				std::map<int, CGroup*>::iterator i;
				std::vector<CGroup*> mergable;

				/* See which groups can be merged already */
				for (i = groups.begin(); i != groups.end(); i++) {
					CGroup *group = i->second;
					float3 dist = group->pos() - pos;
					if (dist.Length2D() <= range)
						mergable.push_back(group);
				}

				/* We have atleast two groups, now we can merge */
				if (merge.size() >= 2) {
					CGroup *alpha = mergable[0];
					for (unsigned j = 1; j < mergable.size(); j++)
						alpha->merge(*mergable[j]);
				}

				/* If only one group remains, merging is no longer possible,
				 * unreg groups 
				 */
				if (groups.size() <= 1) 
					remove();
			}

			/* Reset for reusal */
			void reset() {
				records.clear();
				groups.clear();
			}
		};

		/* Controls which task may be updated (round robin-ish) */
		unsigned update;

		/* The ATask container per task type */
		std::map<task, std::vector<ATask> > taskContainer;

		/* The <task, taskid, vectorid> table */
		std::map<task, std::map<int, int> > lookup;

		/* The free slots per task type */
		std::map<task, std::stack<int> >    free;

		/* The active tasks to update */
		std::map<int, ATask*>               activeTasks;

		/* Add a fresh build task */
		void addBuildTask(float3 &pos, UnitType *toBuild);

		/* Add a fresh assist task */
		void addAssistTask(float3 &pos, UnitType *toBuild);

		/* Add a fresh attack task */
		void addAttackTask(int target);

		/* Add a fresh merge task */
		void addMergeTask(float3 &pos, float range);

		/* Update call */
		void update();

	private:
		AIClasses *ai;
		char buf[1024];
		std::map<task, std::string> taskStr;

		void addTask(task t, ATask &at);
};

#endif
