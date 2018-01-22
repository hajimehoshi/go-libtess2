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
import "C"

// testransEval:
// Given three vertices u,v,w such that TransLeq(u,v) && TransLeq(v,w),
// evaluates the t-coord of the edge uw at the s-coord of the vertex v.
// Returns v.s - (uw)(v.t), ie. the signed distance from uw to v.
// If uw is vertical (and thus passes thru v), the result is zero.
//
// The calculation is extremely accurate and stable, even when v
// is very close to u or w.  In particular if we set v.s = 0 and
// let r be the negated result (this evaluates (uw)(v.t)), then
// r is guaranteed to satisfy MIN(u.s,w.s) <= r <= MAX(u.s,w.s).
func testransEval(u, v, w *C.TESSvertex) C.TESSreal {
	assert(C.TransLeq(u, v) != 0 && C.TransLeq(v, w) != 0)

	gapL := v.t - u.t
	gapR := w.t - v.t

	if gapL+gapR > 0 {
		if gapL < gapR {
			return (v.s - u.s) + (u.s-w.s)*(gapL/(gapL+gapR))
		} else {
			return (v.s - w.s) + (w.s-u.s)*(gapR/(gapL+gapR))
		}
	}
	// vertical line
	return 0
}

// testransSign returns a number whose sign matches TransEval(u,v,w) but which
// is cheaper to evaluate.  Returns > 0, == 0 , or < 0
// as v is above, on, or below the edge uw.
func testransSign(u, v, w *C.TESSvertex) C.TESSreal {
	assert(C.TransLeq(u, v) != 0 && C.TransLeq(v, w) != 0)

	gapL := v.t - u.t
	gapR := w.t - v.t

	if gapL+gapR > 0 {
		return (v.s-w.s)*gapL + (v.s-u.s)*gapR
	}
	// vertical line
	return 0
}

//export tesvertCCW
//
// tesvertCCW:
// For almost-degenerate situations, the results are not reliable.
// Unless the floating-point arithmetic can be performed without
// rounding errors, *any* implementation will give incorrect results
// on some degenerate inputs, so the client must have some way to
// handle this situation.
func tesvertCCW(u, v, w *C.TESSvertex) bool {
	return (u.s*(v.t-w.t) + v.s*(w.t-u.t) + w.s*(u.t-v.t)) >= 0
}

// interpolate:
// Given parameters a,x,b,y returns the value (b*x+a*y)/(a+b),
// or (x+y)/2 if a==b==0.  It requires that a,b >= 0, and enforces
// this in the rare case that one argument is slightly negative.
// The implementation is extremely stable numerically.
// In particular it guarantees that the result r satisfies
// MIN(x,y) <= r <= MAX(x,y), and the results are very accurate
// even when a and b differ greatly in magnitude.
func interpolate(a, x, b, y C.TESSreal) C.TESSreal {
	if a < 0 {
		a = 0
	}
	if b < 0 {
		b = 0
	}
	if a <= b {
		if b == 0 {
			return (x + y) / 2
		}
		return x + (y-x)*(a/(a+b))
	}
	return y + (x-y)*(b/(a+b))
}

// tesedgeIntersect:
// Given edges (o1,d1) and (o2,d2), compute their point of intersection.
// The computed point is guaranteed to lie in the intersection of the
// bounding rectangles defined by each edge.
func tesedgeIntersect(o1 *C.TESSvertex, d1 *C.TESSvertex, o2 *C.TESSvertex, d2 *C.TESSvertex, v *C.TESSvertex) {
	// This is certainly not the most efficient way to find the intersection
	// of two line segments, but it is very numerically stable.
	//
	// Strategy: find the two middle vertices in the VertLeq ordering,
	// and interpolate the intersection s-value from these.  Then repeat
	// using the TransLeq ordering to find the intersection t-value.

	if C.VertLeq(o1, d1) == 0 {
		o1, d1 = d1, o1
	}
	if C.VertLeq(o2, d2) == 0 {
		o2, d2 = d2, o2
	}
	if C.VertLeq(o1, o2) == 0 {
		o1, o2 = o2, o1
		d1, d2 = d2, d1
	}

	if C.VertLeq(o2, d1) == 0 {
		// Technically, no intersection -- do our best
		v.s = (o2.s + d1.s) / 2
	} else if C.VertLeq(d1, d2) != 0 {
		// Interpolate between o2 and d1
		z1 := C.tesedgeEval(o1, o2, d1)
		z2 := C.tesedgeEval(o2, d1, d2)
		if z1+z2 < 0 {
			z1 = -z1
			z2 = -z2
		}
		v.s = interpolate(z1, o2.s, z2, d1.s)
	} else {
		// Interpolate between o2 and d2
		z1 := C.tesedgeSign(o1, o2, d1)
		z2 := -C.tesedgeSign(o1, d2, d1)
		if z1+z2 < 0 {
			z1 = -z1
			z2 = -z2
		}
		v.s = interpolate(z1, o2.s, z2, d2.s)
	}

	// Now repeat the process for t

	if C.TransLeq(o1, d1) == 0 {
		o1, d1 = d1, o1
	}
	if C.TransLeq(o2, d2) == 0 {
		o2, d2 = d2, o2
	}
	if C.TransLeq(o1, o2) == 0 {
		o1, o2 = o2, o1
		d1, d2 = d2, d1
	}

	if C.TransLeq(o2, d1) == 0 {
		// Technically, no intersection -- do our best
		v.t = (o2.t + d1.t) / 2
	} else if C.TransLeq(d1, d2) != 0 {
		// Interpolate between o2 and d1
		z1 := testransEval(o1, o2, d1)
		z2 := testransEval(o2, d1, d2)
		if z1+z2 < 0 {
			z1 = -z1
			z2 = -z2
		}
		v.t = interpolate(z1, o2.t, z2, d1.t)
	} else {
		// Interpolate between o2 and d2
		z1 := testransSign(o1, o2, d1)
		z2 := -testransSign(o1, d2, d1)
		if z1+z2 < 0 {
			z1 = -z1
			z2 = -z2
		}
		v.t = interpolate(z1, o2.t, z2, d2.t)
	}
}
