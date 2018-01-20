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

int ShortAxis( TESSreal* v )
{
	int i = 0;

	if( ABS(v[1]) < ABS(v[0]) ) { i = 1; }
	if( ABS(v[2]) < ABS(v[i]) ) { i = 2; }
	return i;
}
