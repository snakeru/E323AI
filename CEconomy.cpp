#include "CEconomy.h"

#include <cfloat>
#include <limits>

#include "headers/HEngine.h"

#include "CAI.h"
#include "CTaskHandler.h"
#include "CUnitTable.h"
#include "GameMap.hpp"
#include "CWishList.h"
#include "CDefenseMatrix.h"
#include "CGroup.h"
#include "CUnit.h"
#include "CPathfinder.h"
#include "CConfigParser.h"
#include "CIntel.h"
#include "GameMap.hpp"
#include "ReusableObjectFactory.hpp"

CEconomy::CEconomy(AIClasses *ai): ARegistrar(700, std::string("economy")) {
	this->ai = ai;
	latest_factory = 0;
	state = 0;
	incomes  = 0;
	mNow     = mNowSummed     = eNow     = eNowSummed     = 0.0f;
	mIncome  = mIncomeSummed  = eIncome  = eIncomeSummed  = 0.0f;
	uMIncome = uMIncomeSummed = uEIncome = uEIncomeSummed = 0.0f;
	mUsage   = mUsageSummed   = eUsage   = eUsageSummed   = 0.0f;
	mStorage = eStorage                                   = 0.0f;
	mstall = estall = mexceeding = eexceeding = mRequest = eRequest = false;
	initialized = false;
	areMMakersEnabled = ai->gamemap->IsNonMetalMap();
}

CEconomy::~CEconomy()
{
}

void CEconomy::init(CUnit &unit) {
	if(initialized)	return;
	// NOTE: expecting "unit" is a commander unit
	const UnitDef *ud = ai->cb->GetUnitDef(unit.key);
	UnitType *utCommander = UT(ud->id);
	windmap = ((ai->cb->GetMaxWind() + ai->cb->GetMinWind()) / 2.0f) >= 10.0f;
	//float avgWind   = (ai->cb->GetMinWind() + ai->cb->GetMaxWind()) / 2.0f;
	//float windProf  = avgWind / utWind->cost;
	//float solarProf = utSolar->energyMake / utSolar->cost;
	mStart = utCommander->def->metalMake;
	eStart = utCommander->def->energyMake;
	initialized = true;
}
		
bool CEconomy::hasBegunBuilding(CGroup &group) {
	std::map<int, CUnit*>::iterator i;
	for (i = group.units.begin(); i != group.units.end(); i++) {
		CUnit *unit = i->second;
		if (ai->unittable->idle.find(unit->key) == ai->unittable->idle.end()
			|| !ai->unittable->idle[unit->key])
			return true;
	}
	
	return false;
}

bool CEconomy::hasFinishedBuilding(CGroup &group) {
	std::map<int, CUnit*>::iterator i;
	for (i = group.units.begin(); i != group.units.end(); i++) {
		CUnit *unit = i->second;
		if (ai->unittable->builders.find(unit->key) != ai->unittable->builders.end() &&
			ai->unittable->builders[unit->key]) {
			ai->unittable->builders[unit->key] = false;
			return true;
		}
	}
	
	return false;
}

CGroup* CEconomy::requestGroup() {
	CGroup *group = ReusableObjectFactory<CGroup>::Instance();
	group->ai = ai;
	group->reset();
	group->reg(*this);

	activeGroups[group->key] = group;
	
	return group;
}

void CEconomy::remove(ARegistrar &object) {
	CGroup *group = dynamic_cast<CGroup*>(&object);
	LOG_II("CEconomy::remove " << (*group))

	activeGroups.erase(group->key);
	upgradedMexes.erase(group->key);
	takenMexes.erase(group->key);
	takenGeo.erase(group->key);

	group->unreg(*this);
	
	ReusableObjectFactory<CGroup>::Release(group);
}

void CEconomy::addUnitOnCreated(CUnit &unit) {
	unsigned c = unit.type->cats;
	if (c&MEXTRACTOR) {
		CGroup *group = requestGroup();
		group->addUnit(unit);
		takenMexes[group->key] = group->pos();
		if (unit.def->metalCost > 200) upgradedMexes[group->key] = group->pos();
		CUnit *builder = ai->unittable->getUnit(group->firstUnit()->builtBy);
		if (builder) {
			takenMexes.erase(builder->group->key);
			upgradedMexes.erase(builder->group->key);
		}
	}
	else if(unit.type->def->needGeo) {
		CGroup *group = requestGroup();
		group->addUnit(unit);
		takenGeo[group->key] = group->pos();
		CUnit *builder = ai->unittable->getUnit(group->firstUnit()->builtBy);
		if (builder)
			takenGeo.erase(builder->group->key);
	}
}

void CEconomy::addUnitOnFinished(CUnit &unit) {
	LOG_II("CEconomy::addUnitOnFinished " << unit)

	unsigned c = unit.type->cats;

	if (c&BUILDER) {
		CGroup *group = requestGroup();
		group->addUnit(unit);
		if (c&FACTORY) {
			ai->unittable->factories[unit.key] = &unit;
			latest_factory = unit.key;
		}
	}
	else if (c&ASSISTER) {
//		nano turret. Set it to patrol mode and leave it alone
		unit.patrol(ZeroVector);
		unit.move_state(ROAM);
		ai->unittable->nanotowers[unit.key] = &unit;
	}
	else if (c&MMAKER) {
		ai->unittable->metalMakers[unit.key] = &unit;
	}
}

bool CEconomy::buildOrAssist(CGroup &group, buildType bt, unsigned include, unsigned exclude) {
	ATask *task = canAssist(bt, group);
	if (task != NULL) {
		ai->tasks->addAssistTask(*task, group);
		return group.busy;
	}

	/* Retrieve the allowed buildable units */
	CUnit *unit = group.firstUnit();
	std::multimap<float, UnitType*> candidates;
	
	ai->unittable->getBuildables(unit->type, include, exclude, candidates);
	if (candidates.empty()) return false; // builder can build nothing we need

	/* Determine which of these we can afford */
	std::multimap<float, UnitType*>::iterator i;
	/* Determine the location where to build */
	float3 pos = group.pos();
	float3 goal = pos;

	/* Perform the build. Energy building requires separate evaluation. */
	if (bt == BUILD_EPROVIDER) {
                /*
                    1. iterate over candidates
                    2. skip windgens if there is no wind
                    3. skip geotermals if there is no geospots
                    4. put remaining items into second list sorted by: cost - wind_penalty - geo_penalty
                    5. iterate over whole second list, find latest affordable item (it will be most expensive).
                       - if none found - take first w/o penalty
                        - if still none found - take first
                */
		std::multimap<float, UnitType*> candidates2;
		float mult;
		float3 geo_goal = getClosestOpenGeoSpot(group);
		float e = eNow/eStorage;
		for (i = candidates.begin() ; i != candidates.end() ; i++) {
			mult = 1;
			if (i->second->def->windGenerator > EPS) {
				if (!windmap) continue;
				else if (ai->cb->GetCurWind() > 10.0f) mult = 2;
			}
			if (i->second->def->needGeo) {
				if (geo_goal == ZeroVector) continue;
				else if (ai->pathfinder->getETA(group, goal) > 30*150) mult=0.1;
			}

			if (((i->second->def->energyMake - i->second->def->energyUpkeep)*100.0/eIncome)<(estall?3:5)) continue; // ban out too low-tech units

			candidates2.insert(std::pair<float,UnitType*>(float(i->first)*mult, i->second));
		}

		if (candidates2.empty()) {
			takenGeo.erase(group.key);
			return false; // builder can build nothing we need
		}

		for (i = candidates2.begin() ; i != candidates2.end() ; i++)
			if (canAffordToBuild(unit->type, i->second, unit->key)) break;
		if (i == candidates2.end()) i--;
		if (i->second->def->needGeo) goal=geo_goal;
		else takenGeo.erase(group.key);
		ai->tasks->addBuildTask(bt, i->second, group, goal);

		return group.busy;
	}

	bool affordable;
	for (i = candidates.begin();i != candidates.end();i++)
		if ((affordable = canAffordToBuild(unit->type, i->second, unit->key)))
			break;
	

	if (!affordable)
		if (bt == BUILD_MPROVIDER) i--; // build resource provider anyways ...
		else return false;			 // ... other things - only if affordable

	switch(bt) {

		case BUILD_MPROVIDER: {
			if (i->second->def->metalCost > 200)
				goal = getClosestUpgradeableMetalSpot(group);
			else
				goal = getClosestOpenMetalSpot(group);
			UnitType *mmaker = ai->unittable->canBuild(unit->type, LAND|MMAKER);
			/* Very simple choice here - check if we have choice at all */
			if ((goal == ZeroVector) && (mmaker == NULL)) {
				break; // we can't build anything
			}
			bool canBuildMMaker = areMMakersEnabled && ((eIncome - eUsage) >= METAL2ENERGY || eexceeding) && (mmaker != NULL);
			bool willBuildMMaker = false;
			float eta=9999999;
			if (goal != ZeroVector)
				eta = ai->pathfinder->getETA(group, goal);
			else if (canBuildMMaker)
				willBuildMMaker = true;
			else
				return buildOrAssist(group, BUILD_EPROVIDER, EMAKER|LAND);
			/* the choice is not THAT easy. Let's think what should we build - mmaker or mex:
			Decision matrix:
			         eReq&ms eReq&!ms  ms  !ms  eexc&ms eexc&!ms
			Too Far   MMak     mex    MMak MMak  MMak    MMak
			  Far      mex     mex    MMak mex   MMak    MMak
			  Near     mex     mex    mex  mex   mex     mex
			
			    1. if   mspot is near - build mex
			    2. elif eexceeding - build mmak
			    3. elif mspot is too far:
			      3.1 we low on energy - build mex
			      3.2 build mmak
			    4. else (mspot is in mid-range)
			      4.1 not mstalling - build mex
			      4.2 build mak
			*/
			if (willBuildMMaker);                /* already decided*/
			else if (eta < 30*25);               /*  1  */
			else if (eexceeding)                 /*  2  */
				willBuildMMaker = true;
			else if (eta > 30*(unit->def->isCommander?100:250)) {              /*  3  */
				if (eRequest);               /* 3.1 */
				else willBuildMMaker = true; /* 3.2 */
			} else                               /*  4  */
				if (!mstall);                /* 4.1 */
				else willBuildMMaker = true; /* 4.2 */
			
			// hard part of decision is done. Do it
			UnitType* tmp_i = i->second;
			if (willBuildMMaker && canBuildMMaker) {
				/* Check if we can upgrade instead */
				ai->tasks->addBuildTask(bt, mmaker, group, pos);
				takenMexes.erase(group.key);
				upgradedMexes.erase(group.key);
			} else
				ai->tasks->addUpgradeTask(bt, i->second, group, goal);
			break;
		}

		case BUILD_MSTORAGE: case BUILD_ESTORAGE: {
			/* Start building storage after enough ingame time */
			if (!taskInProgress(bt) && ai->cb->GetCurrentFrame() > 30*60*7) {
				pos = ai->defensematrix->getBestDefendedPos(0);
				ai->tasks->addBuildTask(bt, i->second, group, pos);
			}
			break;
		}

		case BUILD_NANOTR: {
			for (i = candidates.begin(); i != candidates.end(); i++) {
				if (i->second->def->onoffable) continue; // nanotowers are not onoffable
				int numFactories = ai->unittable->factories.size();
				CUnit *factory = ai->unittable->getUnit(latest_factory);
				if ((!factory) && numFactories>0) { // latest factory destroyed but we still have more
					factory = ai->unittable->factories.begin()->second;
					latest_factory = factory->key;
				}
				if (factory) pos=factory->group->pos();
				else if (!affordable) break;
				else pos = ai->defensematrix->getBestDefendedPos(0);
				ai->tasks->addBuildTask(bt, i->second, group, pos);
				break;
			}
			break;
		}

		case FACTORY_BUILD: {
			if (!taskInProgress(bt)) {
				int numFactories = ai->unittable->factories.size();

				bool build = numFactories <= 0;
				/*
				if (numFactories > 0) {
					int maxTechLevel = ai->cfgparser->getMaxTechLevel();
					unsigned int cats = getNextFactoryToBuild(unit, maxTechLevel);
					if (cats > 0) {
						cats |= FACTORY;
						UnitType *ut = ai->unittable->getUnitTypeByCats(cats);
						if (ut) {
							if (ut->costMetal > EPS)
								build = ((mNow / ut->costMetal) > 0.8f);
							else
								build = true;
						}
					}
				}
				*/				
				
				float m = mNow / mStorage;
				if ((unit->def->isCommander) && (numFactories <= 0)) affordable = true; // HACK: we should pause builder after completing a task (factory is not affordable if unit just finished building and still has his e/m consumption >0 )
				switch(state) {
				case 0: case 1: case 2: {
					build = (m > 0.4f && affordable);
					break;
				}
				case 3: {
					build = (m > 0.4f);
					break;
				}
				case 4: {
					build = (m > 0.35f);
					break;
				}
				case 5: {
					build = (m > 0.3f);
					break;
				}
				case 6: {
					build = (m > 0.25f);
					break;
				}
				default: {
					build = (m > 0.2f);
				}
				}

				if (build) {
					if (numFactories > 1)
						pos = ai->defensematrix->getBestDefendedPos(numFactories);
					ai->tasks->addBuildTask(bt, i->second, group, pos);
				}
			}
			break;
		}
		
		case BUILD_AG_DEFENSE: case BUILD_AA_DEFENSE: {
			if (!taskInProgress(bt)) {
				pos = ai->defensematrix->getDefenseBuildSite(i->second);
				ai->tasks->addBuildTask(bt, i->second, group, pos);
			}
			break;
		}

		default: {
			if (affordable && !taskInProgress(bt))
				ai->tasks->addBuildTask(bt, i->second, group, goal);
			break;
		}
	}
	return group.busy;
}

float3 CEconomy::getBestSpot(CGroup &group, std::list<float3> &resources, std::map<int, float3> &tracker, bool metal) {
	float bestDist = std::numeric_limits<float>::max();
	float3 bestSpot = ZeroVector;
	float3 gpos = group.pos();
	float radius;
	
	if (metal)
		radius = ai->cb->GetExtractorRadius();
	else
		radius = 16.0f;

	std::list<float3>::iterator i;
	std::map<int, float3>::iterator j;
	for (i = resources.begin(); i != resources.end(); i++) {
		// TODO: compare with actual group properties
		if (i->y < 0.0f)
			continue; // spot is under water
		
		bool taken = false;
		for (j = tracker.begin(); j != tracker.end(); j++) {
			if (i->distance2D(j->second) < radius) {
				taken = true;
				break;
			}
		}
		if (taken) continue; // already taken or scheduled by current AI

		int numUnits = ai->cb->GetFriendlyUnits(&ai->unitIDs[0], *i, 1.1f * radius);
		for (int u = 0; u < numUnits; u++) {
			const int uid = ai->unitIDs[u];
			if (ai->unittable->getUnit(uid)) continue; // it's our own unit. It must have been present in tracker, but it wasn't. Probably it's a mex that is about to be upgraded.
			const UnitDef *ud = ai->cb->GetUnitDef(uid);
			if (metal)
				taken = UC(ud->id) & MEXTRACTOR;
			else
				taken = ud->needGeo;
			if (taken) break;
		}
		if (taken) continue; // already taken by ally team

		// TODO: actually any spot with any threat should be skipped; 
		// to implement this effectively we need to refactor tasks, cause
		// builder during approaching should scan target place for threat
		// periodically; currently it does not, so skipping dangerous spot
		// has no real profit
		float dist = gpos.distance2D(*i);
		dist += 1000.0f * group.getThreat(*i, 300.0f);
		if (dist < bestDist) {
			bestDist = dist;
			bestSpot = *i;
		}
	}

	if (bestSpot != ZeroVector)
		// TODO: improper place for this
		tracker[group.key] = bestSpot;

	return bestSpot;
}

float3 CEconomy::getClosestUpgradeableMetalSpot(CGroup &group) {
	float3 spot = getBestSpot(group, GameMap::metalspots, upgradedMexes, true);
	if (spot != ZeroVector) takenMexes[group.key]=spot;
	return spot;
}

float3 CEconomy::getClosestOpenMetalSpot(CGroup &group) {
	return getBestSpot(group, GameMap::metalspots, takenMexes, true);
}

float3 CEconomy::getClosestOpenGeoSpot(CGroup &group) {
	return getBestSpot(group, GameMap::geospots, takenGeo, false);
}

void CEconomy::commandBuilderGroup(CGroup *group) {
/*
 if mstall or estall - build resources
 build factory (limited amount)
 commander -> help factory
 improve defences
 build storages or mmakers
 build energy if mmakers disabled
 build nanotowers
 help factory
 build more factories
 build more resources
*/
			CUnit *unit = group->firstUnit();
			float3 pos = group->pos();
			/* If we are mstalling deal with it */
			if (mstall)
				if (buildOrAssist(*group, BUILD_MPROVIDER, MEXTRACTOR|LAND)) return;

			/* If we are estalling deal with it */
			if (estall)
				if (buildOrAssist(*group, BUILD_EPROVIDER, EMAKER|LAND)) return;

			/* See if this unit can build desired factory */
			int maxTechLevel = ai->cfgparser->getMaxTechLevel();
			unsigned int factory = getNextFactoryToBuild(unit, maxTechLevel);
			int numOfFactories = ai->unittable->factories.size();
			if ((factory > 0) && (numOfFactories<(1+state/2)))
				if (buildOrAssist(*group, BUILD_FACTORY, factory)) return;

			if (unit->def->isCommander) {
				ATask *task = NULL;
				/* If we can afford to assist a lab and it's close enough, do so */
				if ((task = canAssistFactory(*group)) != NULL)
					ai->tasks->addAssistTask(*task, *group);
				if (group->busy) return;
			}

			/* See if we can build defense */
			if ((numOfFactories>0) && (ai->defensematrix->getClusters()*1.5 > ai->unittable->defenses.size()))
				if (buildOrAssist(*group, BUILD_AG_DEFENSE, DEFENSE, ANTIAIR)) return;

			/* If we are overflowing energy build a estorage */
			if (eexceeding)
				if (ai->unittable->energyStorages.size() >= ai->cfgparser->getMaxTechLevel())
					if (buildOrAssist(*group, BUILD_MPROVIDER, MEXTRACTOR|LAND)) return; // just balance it w/ metal
				else
					if (buildOrAssist(*group, BUILD_ESTORAGE, LAND|ESTORAGE)) return;

			/* If we are overflowing metal build an mstorage */
			if (mexceeding)
				if (buildOrAssist(*group, BUILD_MSTORAGE, LAND|MSTORAGE)) return;

			/* If both requested, see what is required most */
			if (eRequest && mRequest)
				if ((mNow / mStorage) > (eNow / eStorage))
					if (buildOrAssist(*group, BUILD_EPROVIDER, EMAKER|LAND)) return;
				else
					if (buildOrAssist(*group, BUILD_MPROVIDER, MEXTRACTOR|LAND)) return;

			/* Else just provide that which is requested */
			if (eRequest)
				if (buildOrAssist(*group, BUILD_EPROVIDER, EMAKER|LAND)) return;

			if (mRequest)
				if (buildOrAssist(*group, BUILD_MPROVIDER, MEXTRACTOR|LAND)) return;

			/* If we have disabled mmakers - build more energy */
			if (!areMMakersEnabled)
				if (buildOrAssist(*group, BUILD_EPROVIDER, EMAKER|LAND)) return;

			int allowedAssisters = ai->unittable->factories.size()*state/2;
			if (ai->unittable->nanotowers.size()<allowedAssisters)
				if (buildOrAssist(*group, BUILD_NANOTR, LAND|STATIC|ASSISTER, BUILDER)) return;

			ATask *task = NULL;
			/* If we can afford to assist a lab and it's close enough, do so */
			if (ai->unittable->nanotowers.size() < (allowedAssisters/2+1))
				if ((task = canAssistFactory(*group)) != NULL)
					ai->tasks->addAssistTask(*task, *group);
			if (group->busy) return;

			/* if we need more factories - it's good time to build them */
			if (factory > 0)
				if (buildOrAssist(*group, BUILD_FACTORY, factory)) return;

			/* Otherwise just expand */
			if ((mNow / mStorage) > (eNow / eStorage))
				buildOrAssist(*group, BUILD_EPROVIDER, EMAKER|LAND);
			else
				buildOrAssist(*group, BUILD_MPROVIDER, MEXTRACTOR|LAND);
}

void CEconomy::update(int frame) {
	int builderGroupsNum = 0;

	/* See if we can improve our eco by controlling metalmakers */
	controlMetalMakers();

	/* If we are stalling, do something about it */
	preventStalling();

	/* Update idle worker groups */
	std::map<int, CGroup*>::iterator i;
	CUnit *commander = 0;
	for (i = activeGroups.begin(); i != activeGroups.end(); i++) {
		CGroup *group = i->second;
		CUnit *unit = group->firstUnit();

		if (group->cats&MOBILE)
			builderGroupsNum++;

		if (group->busy || !ai->unittable->canPerformTask(*unit))
			continue;

		if (group->cats&FACTORY) {
			ai->tasks->addFactoryTask(*group);
		
			/* TODO: put same factories in a single group. This requires refactoring of task
			assisting, because when factory is dead assisting tasks continue working on ex. factory
			position.

			CUnit *factory = CUnitTable::getUnitByDef(ai->unittable->factories, unit.def);
			if(factory && factory->group) {
				factory->group->addUnit(unit);
			} else {
				CGroup *group = requestGroup();
				group->addUnit(unit);
				ai->tasks->addFactoryTask(*group);
			}
			*/
			
			continue;
		}

		if (group->cats&STATIC) {
			LOG_WW("CEconomy::update AI can't handle static builders except factories")
			continue;
		}

		if (unit->def->isCommander) commander = unit;
		else commandBuilderGroup(group);
	}
	if (commander) commandBuilderGroup(commander->group);

	if (builderGroupsNum < ai->cfgparser->getMaxWorkers() 
	&& (builderGroupsNum < ai->cfgparser->getMinWorkers()))
		ai->wishlist->push(BUILDER, HIGH);
	else if (builderGroupsNum < ai->cfgparser->getMaxWorkers())
		ai->wishlist->push(BUILDER, NORMAL);
}

bool CEconomy::taskInProgress(buildType bt) {
	std::map<int, CTaskHandler::BuildTask*>::iterator i;
	for (i = ai->tasks->activeBuildTasks.begin(); i != ai->tasks->activeBuildTasks.end(); i++) {
		if (i->second->bt == bt)
			return true;
	}
	return false;
}

void CEconomy::controlMetalMakers() {
	float eRatio = eNow / eStorage;
	std::map<int, CUnit*>::iterator j;
	if ((eRatio < 0.3f) || (mexceeding && !eexceeding)) {
		int success = 0;
		for (j = ai->unittable->metalMakers.begin(); j != ai->unittable->metalMakers.end(); j++) {
			CUnit *unit = j->second;
			if(unit->isOn()) {
				unit->setOnOff(false);
				success++;
			}
		}
		if (success > 0) estall = false;
		areMMakersEnabled = false;
		return;
	}

	if (eRatio > 0.7f) {
		int success = 0;
		for (j = ai->unittable->metalMakers.begin(); j != ai->unittable->metalMakers.end(); j++) {
			CUnit *unit = j->second;
			if(!unit->isOn()) {
				unit->setOnOff(true);
				success++;
			}
		}
		if (success > 0) mstall = false;
		areMMakersEnabled = true;
		return;
	}
}

void CEconomy::preventStalling() {
	/* If factorytask is on wait, unwait him */
	std::map<int, CTaskHandler::FactoryTask*>::iterator k;
	for (k = ai->tasks->activeFactoryTasks.begin(); k != ai->tasks->activeFactoryTasks.end(); k++)
		k->second->setWait(false);

	/* If we're not stalling, return */
	if (!mstall && !estall)
		return;

	/* Stop all guarding workers */
	std::map<int,CTaskHandler::AssistTask*>::iterator i;
	for (i = ai->tasks->activeAssistTasks.begin(); i != ai->tasks->activeAssistTasks.end(); i++) {
		/* If the assisting group is moving, continue */
		if (i->second->isMoving)
			continue;

		/* Don't stop those workers that are trying to fix the problem */
		if (i->second->assist->t == BUILD) {
			CTaskHandler::BuildTask* build = dynamic_cast<CTaskHandler::BuildTask*>(i->second->assist);
			if ((mstall || mRequest) && build->bt == BUILD_MPROVIDER)
				continue;

			if ((estall || eRequest) && build->bt == BUILD_EPROVIDER)
				continue;
		}

		/* Don't stop factory assisters, they will be dealt with later */
		if (i->second->assist->t == BUILD && i->second->assist->t != FACTORY_BUILD) {
			i->second->remove();
			return;
		}

		/* Unless it is the commander, he should be fixing the problem */
		if (i->second->group->units.begin()->second->def->isCommander) {
			if ((mstall && !estall) || (estall && !mstall)) {
				i->second->remove();
				return;
			}
		}
	}

	/* Wait all factories and their assisters */
	for (k = ai->tasks->activeFactoryTasks.begin(); k != ai->tasks->activeFactoryTasks.end(); k++)
		k->second->setWait(true);
}

void CEconomy::updateIncomes(int frame) {
	incomes++;

	mNowSummed    += ai->cb->GetMetal();
	eNowSummed    += ai->cb->GetEnergy();
	mIncomeSummed += ai->cb->GetMetalIncome();
	eIncomeSummed += ai->cb->GetEnergyIncome();
	mUsageSummed  += ai->cb->GetMetalUsage();
	eUsageSummed  += ai->cb->GetEnergyUsage();
	mStorage       = ai->cb->GetMetalStorage();
	eStorage       = ai->cb->GetEnergyStorage();

	mNow     = ai->cb->GetMetal();
	eNow     = ai->cb->GetEnergy();
	mIncome  = ai->cb->GetMetalIncome();
	eIncome  = ai->cb->GetEnergyIncome();
	mUsage   = alpha*(mUsageSummed / incomes) + (1.0f-alpha)*(ai->cb->GetMetalUsage());
	eUsage   = beta *(eUsageSummed / incomes) + (1.0f-beta) *(ai->cb->GetEnergyUsage());

	std::map<int, CUnit*>::iterator i;
	float mU = 0.0f, eU = 0.0f;
	for (i = ai->unittable->activeUnits.begin(); i != ai->unittable->activeUnits.end(); i++) {
		unsigned int c = i->second->type->cats;
		if (!(c&MMAKER) || !(c&EMAKER) || !(c&MEXTRACTOR)) {
			mU += i->second->type->metalMake;
			eU += i->second->type->energyMake;
		}
	}
	uMIncomeSummed += mU;
	uEIncomeSummed += eU;
	
	uMIncome   = alpha*(uMIncomeSummed / incomes) + (1.0f-alpha)*mU;
	uEIncome   = beta*(uEIncomeSummed / incomes) + (1.0f-beta)*eU;

	mstall     = (mNow < (mStorage*0.1f) && mUsage > mIncome) || mIncome < (mStart*2.0f) || (mNow < (mIncome-mUsage));
	estall     = (eNow < (eStorage*0.1f) && eUsage > eIncome) || eIncome < (eStart*1.5f) || (eNow < (eIncome-eUsage));

	mexceeding = (mNow > (mStorage*0.9f) && mUsage < mIncome);
	eexceeding = (eNow > (eStorage*0.9f) && eUsage < eIncome);

	mRequest   = (mNow < (mStorage*0.5f));
	eRequest   = (eNow < (eStorage*0.5f));

	int tstate = ai->cfgparser->determineState(mIncome, eIncome);
	if (tstate != state) {
		char buf[255];
		sprintf(buf, "State changed to %d, activated techlevel %d", tstate, ai->cfgparser->getMaxTechLevel());
		LOG_II(buf);
		state = tstate;
	}
}

ATask* CEconomy::canAssist(buildType t, CGroup &group) {
	std::map<int, CTaskHandler::BuildTask*>::iterator i;
	std::multimap<float, CTaskHandler::BuildTask*> suited;

	for (i = ai->tasks->activeBuildTasks.begin(); i != ai->tasks->activeBuildTasks.end(); i++) {
		CTaskHandler::BuildTask *buildtask = i->second;

		/* Only build tasks we are interested in */
		float travelTime;
		if (buildtask->bt != t || !buildtask->assistable(group, travelTime))
			continue;
		
		suited.insert(std::pair<float, CTaskHandler::BuildTask*>(travelTime, buildtask));
	}

	/* There are no suited tasks that require assistance */
	if (suited.empty())
		return NULL;

	bool isCommander = group.firstUnit()->def->isCommander;

	if (isCommander) {
		float3 g = (suited.begin())->second->pos;
		float eta = ai->pathfinder->getETA(group, g);

		/* Don't pursuit as commander when walkdistance is more then 10 seconds */
		if (eta > 30*10) return NULL;
	}

	return suited.begin()->second;
}

ATask* CEconomy::canAssistFactory(CGroup &group) {
	CUnit *unit = group.units.begin()->second;
	std::map<int, CTaskHandler::FactoryTask*>::iterator i;
	std::map<float, CTaskHandler::FactoryTask*> candidates;
	float3 pos = group.pos();

	for (i = ai->tasks->activeFactoryTasks.begin(); i != ai->tasks->activeFactoryTasks.end(); i++) {
		/* TODO: instead of euclid distance, use pathfinder distance */
		float dist = (pos - i->second->pos).Length2D();
		candidates[dist] = i->second;
	}

	if (candidates.empty())
		return NULL;

	if (unit->def->isCommander)
		return candidates.begin()->second;

	std::map<float, CTaskHandler::FactoryTask*>::iterator j;
	for (j = candidates.begin(); j != candidates.end(); j++) {
		if (!j->second->assistable(group))
			continue;
		return j->second;
	}

	return NULL;
}

bool CEconomy::canAffordToBuild(UnitType *builder, UnitType *utToBuild, int unitId) {
	/* NOTE: "Salary" is provided every 32 logical frames */
	float buildTime   = (utToBuild->def->buildTime / builder->def->buildSpeed) * 32.0f;
	float mCost       = utToBuild->def->metalCost;
	float eCost       = utToBuild->def->energyCost;
	float mUnitUsage, eUnitUsage;
	UnitResourceInfo info;
	mUnitUsage = eUnitUsage = 0;
	if (ai->cb->GetUnitResourceInfo(unitId, &info)) {
		mUnitUsage = info.metalUse;
		eUnitUsage = info.energyUse;
	}
	float mPrediction = (mIncome - (mUsage - mUnitUsage) - mCost/buildTime)*buildTime - mCost + mNow;
	float ePrediction = (eIncome - (eUsage - eUnitUsage) - eCost/buildTime)*buildTime - eCost + eNow;
	mRequest          = mPrediction < 0.0f;
	eRequest          = ePrediction < 0.0f;
	return (mPrediction >= 0.0f && ePrediction >= 0.0f && mCost*0.7 < mNow);
}

unsigned int CEconomy::getNextFactoryToBuild(CUnit *unit, int maxteachlevel) {
	if (ai->intel->strategyTechUp) {
		for(std::list<unitCategory>::iterator f = ai->intel->allowedFactories.begin(); f != ai->intel->allowedFactories.end(); f++) {
			for(int techlevel = maxteachlevel; techlevel >= TECH1; techlevel--) {
			int factory = *f|techlevel;
			if(ai->unittable->canBuild(unit->type, factory))
				if(!ai->unittable->gotFactory(factory)) {
					return factory;
				}
			}
		}
	}
	else {	
		for(int techlevel = TECH1; techlevel <= maxteachlevel; techlevel++) {
			for(std::list<unitCategory>::iterator f = ai->intel->allowedFactories.begin(); f != ai->intel->allowedFactories.end(); f++) {
				int factory = *f|techlevel;
				if(ai->unittable->canBuild(unit->type, factory))
					if(!ai->unittable->gotFactory(factory)) {
						return factory;
					}
			}
		}
	}	
	
	return 0;
}
