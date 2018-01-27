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

import (
	"fmt"
)

// WindingRule:
// See OpenGL Red Book for description of the winding rules
// http://www.glprogramming.com/red/chapter11.html
type WindingRule int

const (
	WindingRuleOdd WindingRule = iota
	WindingRuleNonzero
	WindingRulePositive
	WindingRuleNegative
	WindingRuleAbsGeqTwo
)

// The contents of the tessGetElements() depends on element type being passed to tessTesselate().
// Tesselation result element types:
// POLYGONS
//   Each element in the element array is polygon defined as 'polySize' number of vertex indices.
//   If a polygon has than 'polySize' vertices, the remaining indices are stored as UNDEF.
//   Example, drawing a polygon:
//     const int nelems = tessGetElementCount(tess);
//     const TESSindex* elems = tessGetElements(tess);
//     for (int i = 0; i < nelems; i++) {
//         const TESSindex* poly = &elems[i * polySize];
//         glBegin(GL_POLYGON);
//         for (int j = 0; j < polySize; j++) {
//             if (poly[j] == UNDEF) break;
//             glVertex2fv(&verts[poly[j]*vertexSize]);
//         }
//         glEnd();
//     }
//
// CONNECTED_POLYGONS
//   Each element in the element array is polygon defined as 'polySize' number of vertex indices,
//   followed by 'polySize' indices to neighour polygons, that is each element is 'polySize' * 2 indices.
//   If a polygon has than 'polySize' vertices, the remaining indices are stored as UNDEF.
//   If a polygon edge is a boundary, that is, not connected to another polygon, the neighbour index is UNDEF.
//   Example, flood fill based on seed polygon:
//     const int nelems = tessGetElementCount(tess);
//     const TESSindex* elems = tessGetElements(tess);
//     unsigned char* visited = (unsigned char*)calloc(nelems);
//     TESSindex stack[50];
//     int nstack = 0;
//     stack[nstack++] = seedPoly;
//     visited[startPoly] = 1;
//     while (nstack > 0) {
//         TESSindex idx = stack[--nstack];
//			const TESSindex* poly = &elems[idx * polySize * 2];
//			const TESSindex* nei = &poly[polySize];
//          for (int i = 0; i < polySize; i++) {
//              if (poly[i] == UNDEF) break;
//              if (nei[i] != UNDEF && !visited[nei[i]])
//	                stack[nstack++] = nei[i];
//                  visited[nei[i]] = 1;
//              }
//          }
//     }
//
// BOUNDARY_CONTOURS
//   Each element in the element array is [base index, count] pair defining a range of vertices for a contour.
//   The first value is index to first vertex in contour and the second value is number of vertices in the contour.
//   Example, drawing contours:
//     const int nelems = tessGetElementCount(tess);
//     const TESSindex* elems = tessGetElements(tess);
//     for (int i = 0; i < nelems; i++) {
//         const TESSindex base = elems[i * 2];
//         const TESSindex count = elems[i * 2 + 1];
//         glBegin(GL_LINE_LOOP);
//         for (int j = 0; j < count; j++) {
//             glVertex2fv(&verts[(base+j) * vertexSize]);
//         }
//         glEnd();
//     }
//
type elementType int

const (
	elementTypePolygons elementType = iota
	elementTypeConnectedPolygons
	elementTypeBoundaryContours
)

type float float32

type index int

type tesselator struct {
	// state needed for collecting the input data

	// stores the input contours, and eventually
	// the tessellation itself
	mesh *mesh

	// state needed for projecting onto the sweep plane

	normal [3]float // user-specified normal (if provided)
	sUnit  [3]float // unit vector in s-direction (debugging)
	tUnit  [3]float // unit vector in t-direction (debugging)

	bmin [2]float
	bmax [2]float

	// state needed for the line sweep

	windingRule WindingRule // rule for determining polygon interior

	dict  *dict   // edge dictionary for sweep line
	pq    *pq     // priority queue of vertex events
	event *vertex // current sweep event being processed

	vertexIndexCounter index

	vertices      []float
	vertexIndices []index
	vertexCount   int
	elements      []index
	elementCount  int
}

type Vertex struct {
	X float32 // TODO: Replace this to float64 later
	Y float32
	Z float32
}

type Contour []Vertex

func Tesselate(contours []Contour, windingRule WindingRule) ([]int, []Vertex, error) {
	t := &tesselator{}
	for _, c := range contours {
		fs := make([]float32, len(c)*3)
		for i, v := range c {
			fs[3*i] = v.X
			fs[3*i+1] = v.Y
			fs[3*i+2] = v.Z
		}
		tessAddContour(t, 3, fs)
	}

	const (
		polySize   = 3
		vertexSize = 3
	)

	if !tessTesselate(t,
		windingRule,
		elementTypePolygons,
		polySize,
		vertexSize,
		nil) {
		return nil, nil, fmt.Errorf("libtess2: tessTesselate failed")
	}

	vertices := make([]Vertex, t.vertexCount)
	for i := range vertices {
		vertices[i].X = float32(t.vertices[i*3])
		vertices[i].Y = float32(t.vertices[i*3+1])
		vertices[i].Z = float32(t.vertices[i*3+2])
	}

	elements := make([]int, len(t.elements))
	for i, e := range t.elements {
		elements[i] = int(e)
	}
	return elements, vertices, nil
}

func abs(x float) float {
	if x < 0 {
		return -x
	}
	return x
}

func longAxis(v []float) int {
	i := 0
	if abs(v[1]) > abs(v[0]) {
		i = 1
	}
	if abs(v[2]) > abs(v[i]) {
		i = 2
	}
	return i
}

func shortAxis(v []float) int {
	i := 0
	if abs(v[1]) < abs(v[0]) {
		i = 1
	}
	if abs(v[2]) < abs(v[i]) {
		i = 2
	}
	return i
}

func computeNormal(tess *tesselator, norm []float) {
	maxVal := make([]float, 3)
	minVal := make([]float, 3)
	d1 := make([]float, 3)
	d2 := make([]float, 3)
	tNorm := make([]float, 3)
	maxVert := make([]*vertex, 3)
	minVert := make([]*vertex, 3)

	vHead := &tess.mesh.vHead
	v := vHead.next
	for i := 0; i < 3; i++ {
		c := v.coords[i]
		minVal[i] = c
		minVert[i] = v
		maxVal[i] = c
		maxVert[i] = v
	}

	for v := vHead.next; v != vHead; v = v.next {
		for i := 0; i < 3; i++ {
			c := v.coords[i]
			if c < minVal[i] {
				minVal[i] = c
				minVert[i] = v
			}
			if c > maxVal[i] {
				maxVal[i] = c
				maxVert[i] = v
			}
		}
	}

	// Find two vertices separated by at least 1/sqrt(3) of the maximum
	// distance between any two vertices
	i := 0
	if maxVal[1]-minVal[1] > maxVal[0]-minVal[0] {
		i = 1
	}
	if maxVal[2]-minVal[2] > maxVal[i]-minVal[i] {
		i = 2
	}
	if minVal[i] >= maxVal[i] {
		// All vertices are the same -- normal doesn't matter
		norm[0] = 0
		norm[1] = 0
		norm[2] = 1
		return
	}

	// Look for a third vertex which forms the triangle with maximum area
	// (Length of normal == twice the triangle area)
	maxLen2 := float(0)
	v1 := minVert[i]
	v2 := maxVert[i]
	d1[0] = v1.coords[0] - v2.coords[0]
	d1[1] = v1.coords[1] - v2.coords[1]
	d1[2] = v1.coords[2] - v2.coords[2]
	for v := vHead.next; v != vHead; v = v.next {
		d2[0] = v.coords[0] - v2.coords[0]
		d2[1] = v.coords[1] - v2.coords[1]
		d2[2] = v.coords[2] - v2.coords[2]
		tNorm[0] = d1[1]*d2[2] - d1[2]*d2[1]
		tNorm[1] = d1[2]*d2[0] - d1[0]*d2[2]
		tNorm[2] = d1[0]*d2[1] - d1[1]*d2[0]
		tLen2 := tNorm[0]*tNorm[0] + tNorm[1]*tNorm[1] + tNorm[2]*tNorm[2]
		if tLen2 > maxLen2 {
			maxLen2 = tLen2
			norm[0] = tNorm[0]
			norm[1] = tNorm[1]
			norm[2] = tNorm[2]
		}
	}

	if maxLen2 <= 0 {
		// All points lie on a single line -- any decent normal will do
		norm[0] = 0
		norm[1] = 0
		norm[2] = 0
		norm[shortAxis(d1)] = 1
	}
}

func checkOrientation(tess *tesselator) {
	fHead := &tess.mesh.fHead
	vHead := &tess.mesh.vHead

	// When we compute the normal automatically, we choose the orientation
	// so that the the sum of the signed areas of all contours is non-negative.
	area := float(0)
	for f := fHead.next; f != fHead; f = f.next {
		e := f.anEdge
		if e.winding <= 0 {
			continue
		}
		for {
			area += (e.Org.s - e.dst().s) * (e.Org.t + e.dst().t)
			e = e.Lnext
			if e == f.anEdge {
				break
			}
		}
	}
	if area < 0 {
		// Reverse the orientation by flipping all the t-coordinates
		for v := vHead.next; v != vHead; v = v.next {
			v.t = -v.t
		}
		tess.tUnit[0] = -tess.tUnit[0]
		tess.tUnit[1] = -tess.tUnit[1]
		tess.tUnit[2] = -tess.tUnit[2]
	}
}

func dot(u, v []float) float {
	return u[0]*v[0] + u[1]*v[1] + u[2]*v[2]
}

// Determine the polygon normal and project vertices onto the plane
// of the polygon.
func tessProjectPolygon(tess *tesselator) {
	const (
		S_UNIT_X = 1.0
		S_UNIT_Y = 0.0
	)

	vHead := &tess.mesh.vHead
	norm := make([]float, 3)
	computedNormal := false

	norm[0] = tess.normal[0]
	norm[1] = tess.normal[1]
	norm[2] = tess.normal[2]
	if norm[0] == 0 && norm[1] == 0 && norm[2] == 0 {
		computeNormal(tess, norm)
		computedNormal = true
	}
	sUnit := tess.sUnit[:]
	tUnit := tess.tUnit[:]
	i := longAxis(norm)

	// Project perpendicular to a coordinate axis -- better numerically
	sUnit[i] = 0
	sUnit[(i+1)%3] = S_UNIT_X
	sUnit[(i+2)%3] = S_UNIT_Y

	tUnit[i] = 0
	if norm[i] > 0 {
		tUnit[(i+1)%3] = -S_UNIT_Y
	} else {
		tUnit[(i+1)%3] = S_UNIT_Y
	}
	if norm[i] > 0 {
		tUnit[(i+2)%3] = S_UNIT_X
	} else {
		tUnit[(i+2)%3] = -S_UNIT_X
	}

	// Project the vertices onto the sweep plane
	for v := vHead.next; v != vHead; v = v.next {
		v.s = dot(v.coords[:], sUnit)
		v.t = dot(v.coords[:], tUnit)
	}
	if computedNormal {
		checkOrientation(tess)
	}

	// Compute ST bounds.
	first := true
	for v := vHead.next; v != vHead; v = v.next {
		if first {
			tess.bmin[0] = v.s
			tess.bmax[0] = v.s
			tess.bmin[1] = v.t
			tess.bmax[1] = v.t
			first = false
		} else {
			if v.s < tess.bmin[0] {
				tess.bmin[0] = v.s
			}
			if v.s > tess.bmax[0] {
				tess.bmax[0] = v.s
			}
			if v.t < tess.bmin[1] {
				tess.bmin[1] = v.t
			}
			if v.t > tess.bmax[1] {
				tess.bmax[1] = v.t
			}
		}
	}
}

// tessMeshTessellateMonoRegion tessellates a monotone region
// (what else would it do??)  The region must consist of a single
// loop of half-edges (see mesh.h) oriented CCW.  "Monotone" in this
// case means that any vertical line intersects the interior of the
// region in a single interval.
//
// Tessellation consists of adding interior edges (actually pairs of
// half-edges), to split the region into non-overlapping triangles.
//
// The basic idea is explained in Preparata and Shamos (which I don''t
// have handy right now), although their implementation is more
// complicated than this one.  The are two edge chains, an upper chain
// and a lower chain.  We process all vertices from both chains in order,
// from right to left.
//
// The algorithm ensures that the following invariant holds after each
// vertex is processed: the untessellated region consists of two
// chains, where one chain (say the upper) is a single edge, and
// the other chain is concave.  The left vertex of the single edge
// is always to the left of all vertices in the concave chain.
//
// Each step consists of adding the rightmost unprocessed vertex to one
// of the two chains, and forming a fan of triangles from the rightmost
// of two chain endpoints.  Determining whether we can add each triangle
// to the fan is a simple orientation test.  By making the fan as large
// as possible, we restore the invariant (check it yourself).
func tessMeshTessellateMonoRegion(mesh *mesh, face *face) {
	// All edges are oriented CCW around the boundary of the region.
	// First, find the half-edge whose origin vertex is rightmost.
	// Since the sweep goes from left to right, face.anEdge should
	// be close to the edge we want.
	up := face.anEdge
	assert(up.Lnext != up && up.Lnext.Lnext != up)

	for vertLeq(up.dst(), up.Org) {
		up = up.lPrev()
	}
	for vertLeq(up.Org, up.dst()) {
		up = up.Lnext
	}
	lo := up.lPrev()

	for up.Lnext != lo {
		if vertLeq(up.dst(), lo.Org) {
			// up.Dst is on the left.  It is safe to form triangles from lo.Org.
			// The edgeGoesLeft test guarantees progress even when some triangles
			// are CW, given that the upper and lower chains are truly monotone.
			for lo.Lnext != up && (edgeGoesLeft(lo.Lnext) || edgeSign(lo.Org, lo.dst(), lo.Lnext.dst()) <= 0) {
				tempHalfEdge := tessMeshConnect(mesh, lo.Lnext, lo)
				lo = tempHalfEdge.Sym
			}
			lo = lo.lPrev()
		} else {
			// lo.Org is on the left.  We can make CCW triangles from up.Dst.
			for lo.Lnext != up && (edgeGoesRight(up.lPrev()) || edgeSign(up.dst(), up.Org, up.lPrev().Org) >= 0) {
				tempHalfEdge := tessMeshConnect(mesh, up, up.lPrev())
				up = tempHalfEdge.Sym
			}
			up = up.Lnext
		}
	}

	// Now lo.Org == up.Dst == the leftmost vertex.  The remaining region
	// can be tessellated in a fan from this leftmost vertex.
	assert(lo.Lnext != up)
	for lo.Lnext.Lnext != up {
		tempHalfEdge := tessMeshConnect(mesh, lo.Lnext, lo)
		lo = tempHalfEdge.Sym
	}
}

// tessMeshTessellateInterior tessellates each region of
// the mesh which is marked "inside" the polygon.  Each such region
// must be monotone.
func tessMeshTessellateInterior(mesh *mesh) {
	var next *face
	for f := mesh.fHead.next; f != &mesh.fHead; f = next {
		// Make sure we don't try to tessellate the new triangles.
		next = f.next
		if f.inside {
			tessMeshTessellateMonoRegion(mesh, f)
		}
	}
}

// tessMeshSetWindingNumber resets the
// winding numbers on all edges so that regions marked "inside" the
// polygon have a winding number of "value", and regions outside
// have a winding number of 0.
//
// If keepOnlyBoundary is TRUE, it also deletes all edges which do not
// separate an interior region from an exterior one.
func tessMeshSetWindingNumber(mesh *mesh, value int, keepOnlyBoundary bool) {
	var eNext *halfEdge
	for e := mesh.eHead.next; e != &mesh.eHead; e = eNext {
		eNext = e.next
		if e.rFace().inside != e.Lface.inside {
			// This is a boundary edge (one side is interior, one is exterior).
			e.winding = 0
			if e.Lface.inside {
				e.winding = value
			} else {
				e.winding = -value
			}
		} else {

			// Both regions are interior, or both are exterior.
			if !keepOnlyBoundary {
				e.winding = 0
			} else {
				tessMeshDelete(mesh, e)
			}
		}
	}
}

func neighbourFace(edge *halfEdge) index {
	if edge.rFace() == nil {
		return undef
	}
	if !edge.rFace().inside {
		return undef
	}
	return edge.rFace().n
}

func outputPolymesh(tess *tesselator, mesh *mesh, elementType elementType, polySize int, vertexSize int) {
	// Assume that the input data is triangles now.
	// Try to merge as many polygons as possible
	if polySize > 3 {
		tessMeshMergeConvexFaces(mesh, polySize)
	}

	// Mark unused
	for v := mesh.vHead.next; v != &mesh.vHead; v = v.next {
		v.n = undef
	}

	maxFaceCount := 0
	maxVertexCount := 0

	// Create unique IDs for all vertices and faces.
	for f := mesh.fHead.next; f != &mesh.fHead; f = f.next {
		f.n = undef
		if !f.inside {
			continue
		}

		edge := f.anEdge
		faceVerts := 0
		for {
			v := edge.Org
			if v.n == undef {
				v.n = index(maxVertexCount)
				maxVertexCount++
			}
			faceVerts++
			edge = edge.Lnext
			if edge == f.anEdge {
				break
			}
		}
		assert(faceVerts <= polySize)
		f.n = index(maxFaceCount)
		maxFaceCount++
	}

	tess.elementCount = maxFaceCount
	if elementType == elementTypeConnectedPolygons {
		maxFaceCount *= 2
	}
	tess.elements = make([]index, maxFaceCount*polySize)
	tess.vertexCount = maxVertexCount
	tess.vertices = make([]float, int(tess.vertexCount)*vertexSize)
	tess.vertexIndices = make([]index, tess.vertexCount)

	// Output vertices.
	for v := mesh.vHead.next; v != &mesh.vHead; v = v.next {
		if v.n != undef {
			// Store coordinate
			tess.vertices[int(v.n)*vertexSize] = v.coords[0]
			tess.vertices[int(v.n)*vertexSize+1] = v.coords[1]
			if vertexSize > 2 {
				tess.vertices[int(v.n)*vertexSize+2] = v.coords[2]
			}
			// Store vertex index.
			tess.vertexIndices[v.n] = v.idx
		}
	}

	// Output indices.
	elements := tess.elements
	for f := mesh.fHead.next; f != &mesh.fHead; f = f.next {
		if !f.inside {
			continue
		}

		// Store polygon
		edge := f.anEdge
		faceVerts := 0
		for {
			v := edge.Org
			elements[0] = v.n
			elements = elements[1:]
			faceVerts++
			edge = edge.Lnext
			if edge == f.anEdge {
				break
			}
		}
		// Fill unused.
		for i := faceVerts; i < polySize; i++ {
			elements[0] = undef
			elements = elements[1:]
		}

		// Store polygon connectivity
		if elementType == elementTypeConnectedPolygons {
			edge := f.anEdge
			for {
				elements[0] = neighbourFace(edge)
				elements = elements[1:]
				edge = edge.Lnext
				if edge == f.anEdge {
					break
				}
			}
			// Fill unused.
			for i := faceVerts; i < polySize; i++ {
				elements[0] = undef
				elements = elements[1:]
			}
		}
	}
}

func outputContours(tess *tesselator, mesh *mesh, vertexSize int) {
	tess.vertexCount = 0
	tess.elementCount = 0

	for f := mesh.fHead.next; f != &mesh.fHead; f = f.next {
		if !f.inside {
			continue
		}

		start := f.anEdge
		edge := f.anEdge
		for {
			tess.vertexCount++
			edge = edge.Lnext
			if edge == start {
				break
			}
		}
		tess.elementCount++
	}

	tess.elements = make([]index, tess.elementCount*2)
	tess.vertices = make([]float, int(tess.vertexCount)*vertexSize)
	tess.vertexIndices = make([]index, int(tess.vertexCount))

	verts := tess.vertices
	elements := tess.elements
	vertInds := tess.vertexIndices

	startVert := 0

	for f := mesh.fHead.next; f != &mesh.fHead; f = f.next {
		if !f.inside {
			continue
		}

		vertCount := 0
		start := f.anEdge
		edge := f.anEdge
		for {
			verts[0] = edge.Org.coords[0]
			verts = verts[1:]
			verts[0] = edge.Org.coords[1]
			verts = verts[1:]
			if vertexSize > 2 {
				verts[0] = edge.Org.coords[2]
				verts = verts[1:]
			}
			vertInds[0] = edge.Org.idx
			vertInds = vertInds[1:]
			vertCount++
			edge = edge.Lnext
			if edge == start {
				break
			}
		}

		elements[0] = index(startVert)
		elements[1] = index(vertCount)
		elements = elements[2:]

		startVert += vertCount
	}
}

// tessAddContour: - Adds a contour to be tesselated.
// The type of the vertex coordinates is assumed to be TESSfloat.
// Parameters:
//   tess - pointer to tesselator object.
//   size - number of coordinates per vertex. Must be 2 or 3.
//   vertices - vertices array
func tessAddContour(tess *tesselator, size int, vertices []float32) {
	if tess.mesh == nil {
		tess.mesh = tessMeshNewMesh()
	}

	if size < 2 {
		size = 2
	}
	if size > 3 {
		size = 3
	}

	var e *halfEdge
	numVertices := len(vertices) / size
	src := vertices
	for i := 0; i < numVertices; i++ {
		coords := src
		src = src[size:]

		if e == nil {
			// Make a self-loop (one vertex, one edge).
			e = tessMeshMakeEdge(tess.mesh)
			tessMeshSplice(tess.mesh, e, e.Sym)
		} else {
			// Create a new vertex and edge which immediately follow e
			// in the ordering around the left face.
			tessMeshSplitEdge(tess.mesh, e)
			e = e.Lnext
		}

		// The new vertex is now e.Org.
		e.Org.coords[0] = float(coords[0])
		e.Org.coords[1] = float(coords[1])
		if size > 2 {
			e.Org.coords[2] = float(coords[2])
		} else {
			e.Org.coords[2] = 0
		}
		// Store the insertion number so that the vertex can be later recognized.
		e.Org.idx = tess.vertexIndexCounter
		tess.vertexIndexCounter++

		// The winding of an edge says how the winding number changes as we
		// cross from the edge''s right face to its left face.  We add the
		// vertices in such an order that a CCW contour will add +1 to
		// the winding number of the region inside the contour.
		e.winding = 1
		e.Sym.winding = -1
	}
}

// tessTesselate: - tesselate contours.
// Parameters:
//   tess - pointer to tesselator object.
//   windingRule - winding rules used for tesselation, must be one of TessWindingRule.
//   elementType - defines the tesselation result element type, must be one of TessElementType.
//   polySize - defines maximum vertices per polygons if output is polygons.
//   vertexSize - defines the number of coordinates in tesselation result vertex, must be 2 or 3.
//   normal - defines the normal of the input contours, of null the normal is calculated automatically.
// Returns:
//   true if succeed, false if failed.
func tessTesselate(tess *tesselator, windingRule WindingRule, elementType elementType, polySize int, vertexSize int, normal []float) bool {
	tess.vertexIndexCounter = 0

	if normal != nil {
		copy(tess.normal[:], normal[:])
	}

	tess.windingRule = windingRule

	if vertexSize < 2 {
		vertexSize = 2
	}
	if vertexSize > 3 {
		vertexSize = 3
	}

	if tess.mesh == nil {
		return false
	}

	// Determine the polygon normal and project vertices onto the plane
	// of the polygon.
	tessProjectPolygon(tess)

	// tessComputeInterior( tess ) computes the planar arrangement specified
	// by the given contours, and further subdivides this arrangement
	// into regions.  Each region is marked "inside" if it belongs
	// to the polygon, according to the rule given by tess.windingRule.
	// Each interior region is guaranteed be monotone.
	tessComputeInterior(tess)

	mesh := tess.mesh

	// If the user wants only the boundary contours, we throw away all edges
	// except those which separate the interior from the exterior.
	// Otherwise we tessellate all the regions marked "inside".
	if elementType == elementTypeBoundaryContours {
		tessMeshSetWindingNumber(mesh, 1, true)
	} else {
		tessMeshTessellateInterior(mesh)
	}

	tessMeshCheckMesh(mesh)

	if elementType == elementTypeBoundaryContours {
		// output contours
		outputContours(tess, mesh, vertexSize)
	} else {
		// output polygons
		outputPolymesh(tess, mesh, elementType, polySize, vertexSize)
	}

	tess.mesh = nil

	return true
}
