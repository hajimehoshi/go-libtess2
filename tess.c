/*
** SGI FREE SOFTWARE LICENSE B (Version 2.0, Sept. 18, 2008) 
** Copyright (C) [dates of first publication] Silicon Graphics, Inc.
** All Rights Reserved.
**
** Permission is hereby granted, free of charge, to any person obtaining a copy
** of this software and associated documentation files (the "Software"), to deal
** in the Software without restriction, including without limitation the rights
** to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
** of the Software, and to permit persons to whom the Software is furnished to do so,
** subject to the following conditions:
** 
** The above copyright notice including the dates of first publication and either this
** permission notice or a reference to http://oss.sgi.com/projects/FreeB/ shall be
** included in all copies or substantial portions of the Software. 
**
** THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
** INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
** PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL SILICON GRAPHICS, INC.
** BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
** TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE
** OR OTHER DEALINGS IN THE SOFTWARE.
** 
** Except as contained in this notice, the name of Silicon Graphics, Inc. shall not
** be used in advertising or otherwise to promote the sale, use or other dealings in
** this Software without prior written authorization from Silicon Graphics, Inc.
*/
/*
** Author: Eric Veach, July 1994.
*/

#include <stddef.h>
#include <assert.h>
#include <setjmp.h>
#include "bucketalloc.h"
#include "tess.h"
#include "mesh.h"
#include "sweep.h"
#include "geom.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define TRUE 1
#define FALSE 0

#if defined(FOR_TRITE_TEST_PROGRAM) || defined(TRUE_PROJECT)
static void Normalize( TESSreal v[3] )
{
	TESSreal len = v[0]*v[0] + v[1]*v[1] + v[2]*v[2];

	assert( len > 0 );
	len = sqrtf( len );
	v[0] /= len;
	v[1] /= len;
	v[2] /= len;
}
#endif

#define ABS(x)	((x) < 0 ? -(x) : (x))

int LongAxis( TESSreal* v )
{
	int i = 0;

	if( ABS(v[1]) > ABS(v[0]) ) { i = 1; }
	if( ABS(v[2]) > ABS(v[i]) ) { i = 2; }
	return i;
}

static int ShortAxis( TESSreal v[3] )
{
	int i = 0;

	if( ABS(v[1]) < ABS(v[0]) ) { i = 1; }
	if( ABS(v[2]) < ABS(v[i]) ) { i = 2; }
	return i;
}

void ComputeNormal( TESStesselator *tess, TESSreal norm[3] )
{
	TESSvertex *v, *v1, *v2;
	TESSreal c, tLen2, maxLen2;
	TESSreal maxVal[3], minVal[3], d1[3], d2[3], tNorm[3];
	TESSvertex *maxVert[3], *minVert[3];
	TESSvertex *vHead = &tess->mesh->vHead;
	int i;

	v = vHead->next;
	for( i = 0; i < 3; ++i ) {
		c = v->coords[i];
		minVal[i] = c;
		minVert[i] = v;
		maxVal[i] = c;
		maxVert[i] = v;
	}

	for( v = vHead->next; v != vHead; v = v->next ) {
		for( i = 0; i < 3; ++i ) {
			c = v->coords[i];
			if( c < minVal[i] ) { minVal[i] = c; minVert[i] = v; }
			if( c > maxVal[i] ) { maxVal[i] = c; maxVert[i] = v; }
		}
	}

	/* Find two vertices separated by at least 1/sqrt(3) of the maximum
	* distance between any two vertices
	*/
	i = 0;
	if( maxVal[1] - minVal[1] > maxVal[0] - minVal[0] ) { i = 1; }
	if( maxVal[2] - minVal[2] > maxVal[i] - minVal[i] ) { i = 2; }
	if( minVal[i] >= maxVal[i] ) {
		/* All vertices are the same -- normal doesn't matter */
		norm[0] = 0; norm[1] = 0; norm[2] = 1;
		return;
	}

	/* Look for a third vertex which forms the triangle with maximum area
	* (Length of normal == twice the triangle area)
	*/
	maxLen2 = 0;
	v1 = minVert[i];
	v2 = maxVert[i];
	d1[0] = v1->coords[0] - v2->coords[0];
	d1[1] = v1->coords[1] - v2->coords[1];
	d1[2] = v1->coords[2] - v2->coords[2];
	for( v = vHead->next; v != vHead; v = v->next ) {
		d2[0] = v->coords[0] - v2->coords[0];
		d2[1] = v->coords[1] - v2->coords[1];
		d2[2] = v->coords[2] - v2->coords[2];
		tNorm[0] = d1[1]*d2[2] - d1[2]*d2[1];
		tNorm[1] = d1[2]*d2[0] - d1[0]*d2[2];
		tNorm[2] = d1[0]*d2[1] - d1[1]*d2[0];
		tLen2 = tNorm[0]*tNorm[0] + tNorm[1]*tNorm[1] + tNorm[2]*tNorm[2];
		if( tLen2 > maxLen2 ) {
			maxLen2 = tLen2;
			norm[0] = tNorm[0];
			norm[1] = tNorm[1];
			norm[2] = tNorm[2];
		}
	}

	if( maxLen2 <= 0 ) {
		/* All points lie on a single line -- any decent normal will do */
		norm[0] = norm[1] = norm[2] = 0;
		norm[ShortAxis(d1)] = 1;
	}
}
