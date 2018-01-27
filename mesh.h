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

#ifndef MESH_H
#define MESH_H

#include "tesselator.h"

typedef struct TESSmesh TESSmesh; 
typedef struct TESSvertex TESSvertex;
typedef struct TESSface TESSface;
typedef struct TESShalfEdge TESShalfEdge;

struct TESSvertex {
	TESSvertex *next;      /* next vertex (never NULL) */
	TESSvertex *prev;      /* previous vertex (never NULL) */
	TESShalfEdge *anEdge;    /* a half-edge with this origin */

	/* Internal data (keep hidden) */
	TESSreal coords[3];  /* vertex location in 3D */
	TESSreal s, t;       /* projection onto the sweep plane */
	TESSvertex* pqHandle;   /* to allow deletion from priority queue */
	TESSindex n;			/* to allow identify unique vertices */
	TESSindex idx;			/* to allow map result to original verts */
};

struct TESSface {
	TESSface *next;      /* next face (never NULL) */
	TESSface *prev;      /* previous face (never NULL) */
	TESShalfEdge *anEdge;    /* a half edge with this left face */

	/* Internal data (keep hidden) */
	TESSface *trail;     /* "stack" for conversion to strips */
	TESSindex n;		/* to allow identiy unique faces */
	char marked;     /* flag for conversion to strips */
	char inside;     /* this face is in the polygon interior */
};

struct TESShalfEdge {
	TESShalfEdge *next;      /* doubly-linked list (prev==Sym->next) */
	TESShalfEdge *Sym;       /* same edge, opposite direction */
	TESShalfEdge *Onext;     /* next edge CCW around origin */
	TESShalfEdge *Lnext;     /* next edge CCW around left face */
	TESSvertex *Org;       /* origin vertex (Overtex too long) */
	TESSface *Lface;     /* left face */

	/* Internal data (keep hidden) */
	void* activeRegion;  /* a region with this upper edge (sweep.c) */
	int winding;    /* change in winding number when crossing
						  from the right face to the left face */
};

#endif
