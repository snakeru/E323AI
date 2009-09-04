#ifndef THREATMAP_H
#define THREATMAP_H

#include "headers/HEngine.h"

class AIClasses;

class CThreatMap {
	public:
		CThreatMap(AIClasses *ai);
		~CThreatMap();

		void update(int frame);
		float getThreat(float3 &pos);
		float getThreat(float3 &center, float radius);
		int X, Z;
		float *map;
		int RES;

	private:
		AIClasses *ai;	

		void draw();

		int   *units;
		float totalPower;
		float REAL;
};

#endif
