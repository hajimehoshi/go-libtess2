// SGI FREE SOFTWARE LICENSE B (Version 2.0, Sept. 18, 2008)
// Copyright (C) [dates of first publication] Silicon Graphics, Inc.
// All Rights Reserved.
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is furnished to do so,
// subject to the following conditions:
//
// The above copyright notice including the dates of first publication and either this
// permission notice or a reference to http://oss.sgi.com/projects/FreeB/ shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
// INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
// PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL SILICON GRAPHICS, INC.
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
// TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
// OR OTHER DEALINGS IN THE SOFTWARE.
//
// Except as contained in this notice, the name of Silicon Graphics, Inc. shall not
// be used in advertising or otherwise to promote the sale, use or other dealings in
// this Software without prior written authorization from Silicon Graphics, Inc.

package libtess2

// #include "geom.h"
// #include "mesh.h"
// #include "priorityq.h"
// #include "sweep.h"
// #include "tess.h"
// #include "tesselator.h"
//
// void DoneEdgeDict( TESStesselator *tess );
// void InitEdgeDict( TESStesselator *tess );
// int InitPriorityQ( TESStesselator *tess );
// int RemoveDegenerateFaces( TESStesselator *tess, TESSmesh *mesh );
// void RemoveDegenerateEdges( TESStesselator *tess );
// void SpliceMergeVertices( TESStesselator *tess, TESShalfEdge *e1, TESShalfEdge *e2 );
// void SweepEvent( TESStesselator *tess, TESSvertex *vEvent );
import "C"

func donePriorityQ(tess *C.TESStesselator) {
	pqDeletePriorityQ(tess.pq)
}

//export tessComputeInterior
//
// tessComputeInterior( tess ) computes the planar arrangement specified
// by the given contours, and further subdivides this arrangement
// into regions.  Each region is marked "inside" if it belongs
// to the polygon, according to the rule given by tess.windingRule.
// Each interior region is guaranteed be monotone.
func tessComputeInterior(tess *C.TESStesselator) C.int {
	//TESSvertex *v, *vNext;

	// Each vertex defines an event for our sweep line.  Start by inserting
	// all the vertices in a priority queue.  Events are processed in
	// lexicographic order, ie.
	//
	//	e1 < e2  iff  e1.x < e2.x || (e1.x == e2.x && e1.y < e2.y)
	C.RemoveDegenerateEdges(tess)
	if C.InitPriorityQ(tess) == 0 {
		return 0
	}
	C.InitEdgeDict(tess)

	for {
		v := (*C.TESSvertex)(C.pqExtractMin(tess.pq))
		if v == nil {
			break
		}
		for {
			vNext := (*C.TESSvertex)(C.pqMinimum(tess.pq))
			if vNext == nil || C.VertEq(vNext, v) == 0 {
				break
			}

			// Merge together all vertices at exactly the same location.
			// This is more efficient than processing them one at a time,
			// simplifies the code (see ConnectLeftDegenerate), and is also
			// important for correct handling of certain degenerate cases.
			// For example, suppose there are two identical edges A and B
			// that belong to different contours (so without this code they would
			// be processed by separate sweep events).  Suppose another edge C
			// crosses A and B from above.  When A is processed, we split it
			// at its intersection point with C.  However this also splits C,
			// so when we insert B we may compute a slightly different
			// intersection point.  This might leave two edges with a small
			// gap between them.  This kind of error is especially obvious
			// when using boundary extraction (TESS_BOUNDARY_ONLY).
			vNext = (*C.TESSvertex)(C.pqExtractMin(tess.pq))
			C.SpliceMergeVertices(tess, v.anEdge, vNext.anEdge)
		}
		C.SweepEvent(tess, v)
	}

	// Set tess.event for debugging purposes
	//tess.event = dictKey(dictMin(tess.dict)).eUp.Org
	//C.DebugEvent(tess)
	C.DoneEdgeDict(tess)
	donePriorityQ(tess)

	if C.RemoveDegenerateFaces(tess, tess.mesh) == 0 {
		return 0
	}
	C.tessMeshCheckMesh(tess.mesh)

	return 1
}
