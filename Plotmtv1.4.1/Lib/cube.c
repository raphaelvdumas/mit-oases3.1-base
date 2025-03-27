/*
 * cube.c - linked list procedures building and maintaining lists
 *          of cubes 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include <math.h>
#include "CNplot.h"

static void print_cube();
static void print_block();

/*
 * Query functions
 */

/*
 * Is the cube divided into prisms?
 */
int CNcube_is_divided(cube)
CNcubeptr cube;
{
   int divided = CN_FALSE;

   /* Check 1st 3 bits */
   if ( ((cube->pcode & CN_XPRISM) != 0) ||
        ((cube->pcode & CN_YPRISM) != 0) ||
        ((cube->pcode & CN_ZPRISM) != 0) )
      divided = CN_TRUE;

   /* Check the materials just to make sure */
   if (divided)
      if (cube->mat1 == cube->mat2) divided = CN_FALSE;

   /* Return */
   return(divided);
}

/*
 * Is the xmin face of the cube an electrode?
 */
int CNcube_xmin_is_electrode(cube)
CNcubeptr cube;
{
   int electrode = CN_FALSE;

   /* Check the relevant bit */
   if ((cube->pcode & CN_XMINELECT) != 0) 
      electrode = CN_TRUE;

   /* Return */
   return(electrode);
}

/*
 * Is the xmax face of the cube an electrode?
 */
int CNcube_xmax_is_electrode(cube)
CNcubeptr cube;
{
   int electrode = CN_FALSE;

   /* Check the relevant bit */
   if ((cube->pcode & CN_XMAXELECT) != 0) 
      electrode = CN_TRUE;

   /* Return */
   return(electrode);
}

/*
 * Is the ymin face of the cube an electrode?
 */
int CNcube_ymin_is_electrode(cube)
CNcubeptr cube;
{
   int electrode = CN_FALSE;

   /* Check the relevant bit */
   if ((cube->pcode & CN_YMINELECT) != 0) 
      electrode = CN_TRUE;

   /* Return */
   return(electrode);
}

/*
 * Is the ymax face of the cube an electrode?
 */
int CNcube_ymax_is_electrode(cube)
CNcubeptr cube;
{
   int electrode = CN_FALSE;

   /* Check the relevant bit */
   if ((cube->pcode & CN_YMAXELECT) != 0) 
      electrode = CN_TRUE;

   /* Return */
   return(electrode);
}

/*
 * Is the zmin face of the cube an electrode?
 */
int CNcube_zmin_is_electrode(cube)
CNcubeptr cube;
{
   int electrode = CN_FALSE;

   /* Check the relevant bit */
   if ((cube->pcode & CN_ZMINELECT) != 0) 
      electrode = CN_TRUE;

   /* Return */
   return(electrode);
}

/*
 * Is the zmax face of the cube an electrode?
 */
int CNcube_zmax_is_electrode(cube)
CNcubeptr cube;
{
   int electrode = CN_FALSE;

   /* Check the relevant bit */
   if ((cube->pcode & CN_ZMAXELECT) != 0) 
      electrode = CN_TRUE;

   /* Return */
   return(electrode);
}

/*
 * Is the diag face of the cube an electrode?
 */
int CNcube_diag_is_electrode(cube)
CNcubeptr cube;
{
   int electrode = CN_FALSE;

   /* Check the relevant bit */
   if ((cube->pcode & CN_DIAGELECT) != 0) 
      electrode = CN_TRUE;

   /* Return */
   return(electrode);
}


/*
 * CUBE DATA STRUCTURE
 */

/*
 * A cube is used to represent a geometrical block structure.
 * Internally, the cube could be divided into 2 prisms where
 * the cut is parallel to the x, y, or z axis.
 */

/*
 * Allocate room for a cube
 */
CNcubeptr CNmake_cube(x0, x1, y0, y1, z0, z1,
                      mat1,mat2,prism_code,ID)
double x0, x1, y0, y1, z0, z1;
short  mat1,mat2;
int    prism_code;
short  ID;
{
   CNcubeptr newptr;
   unsigned int size = sizeof(CNcube);

   if ((newptr = (CNcubeptr)malloc(size))!=NULL) {
      newptr->ID      = ID;
      newptr->mat1    = mat1;
      newptr->mat2    = mat2;
      newptr->pcode   = prism_code;
      newptr->zave    = 0.0;
      newptr->x0      = x0;
      newptr->x1      = x1;
      newptr->y0      = y0;
      newptr->y1      = y1;
      newptr->z0      = z0;
      newptr->z1      = z1;
      newptr->x0_o    = x0;
      newptr->x1_o    = x1;
      newptr->y0_o    = y0;
      newptr->y1_o    = y1;
      newptr->z0_o    = z0;
      newptr->z1_o    = z1;
      newptr->next    = NULL;
      newptr->prev    = NULL;
   }
   return(newptr);
}

/*  
 * Insert a cube at the tail of the current cube list
 */
CNcubeptr CNinsert_cube(cube_listhead, cube_listtail, 
                        x0, x1, y0, y1, z0, z1,
                        mat1,mat2,prism_code,ID)
CNcubeptr *cube_listhead, *cube_listtail;
double    x0, x1, y0, y1, z0, z1;
short     mat1,mat2;
int       prism_code;
short     ID;
{
   CNcubeptr CNmake_cube();
   CNcubeptr next,A,B;

   A = *cube_listtail;
   if ((B=CNmake_cube(x0, x1, y0, y1, z0, z1,
                      mat1,mat2,prism_code,ID))!=NULL) {
      if (A==NULL) {
         *cube_listhead = B;
         *cube_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next != NULL) next->prev = B;
         if (B->next == NULL) *cube_listtail = B;
      }
   }
   return(B);
}


/*
 * Delete cube at address L 
 */
void CNdelete_cube(cube_listhead, cube_listtail, L)
CNcubeptr *cube_listhead, *cube_listtail;
CNcubeptr L;
{
   CNcubeptr prev,next;

   prev = L->prev;
   next = L->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (L== *cube_listhead) *cube_listhead = next;
   if (L== *cube_listtail) *cube_listtail = prev;
   free ((char*)L);
   L = NULL;
}


/* 
 * Delete all the cubes in the list
 */
void CNdelete_cube_list(cube_listhead, cube_listtail)
CNcubeptr *cube_listhead, *cube_listtail;
{
   CNcubeptr C;

   while ((C = *cube_listhead) != NULL)
      CNdelete_cube(cube_listhead, cube_listtail, C);
}


/* 
 * Print out the list of cubes 
 */
/*ARGSUSED*/
void CNprint_cube_list(cube_listhead, cube_listtail)
CNcubeptr cube_listhead, cube_listtail;
{
   CNcubeptr C;

   for (C=cube_listhead; C!=NULL; C=C->next) 
      print_cube(C);
}


/* 
 * print the coordinates of a cube
 */
static void print_cube(C)
CNcubeptr C;
{
   (void) fprintf(stdout,"CubeID=%d\n",C->ID);
   (void) fprintf(stdout,"   x0=%8.5g x1=%8.5g\n",C->x0, C->x1);
   (void) fprintf(stdout,"   y0=%8.5g y1=%8.5g\n",C->y0, C->y1);
   (void) fprintf(stdout,"   z0=%8.5g z1=%8.5g\n",C->z0, C->z1);
}


/*
 * Count the number of cubes in the list 
 */
/*ARGSUSED*/
int CNcount_cubes(cube_listhead, cube_listtail)
CNcubeptr cube_listhead, cube_listtail;
{
   CNcubeptr C;
   int       count = 0;

   for (C=cube_listhead; C!=NULL; C=C->next) count++;

   return(count);
}


/*
 * BLOCK DATA STRUCTURE
 */

/*
 * A block is used to represent 4D data, that is, values in 3D space
 * on each of the vertices of the cube.  Internally, the block could
 * be divided into 2 prisms where the cut is parallel to the x, y, or z axis.
 * The block refers to the cube for its geometry.
 * Multiple blocks could point to the same cube.
 * Thus the block-cube relation is analogous to the node-point relationship.
 */

/*
 * Allocate room for a block
 */
CNblockptr CNmake_block(cube,t000,t001,t010,t011,t100,t101,t110,t111,ID)
CNcubeptr cube;
double    t000,t001,t010,t011,t100,t101,t110,t111;
int       ID;
{
   CNblockptr newptr;
   unsigned int size = sizeof(CNblock);

   if ((newptr = (CNblockptr)malloc(size))!=NULL) {
      newptr->ID     = ID;
      newptr->cube   = cube;
      newptr->zave   = 0.0;
      newptr->t000   = t000;
      newptr->t001   = t001;
      newptr->t010   = t010;
      newptr->t011   = t011;
      newptr->t100   = t100;
      newptr->t101   = t101;
      newptr->t110   = t110;
      newptr->t111   = t111;
      newptr->next   = NULL;
      newptr->prev   = NULL;
   }
   return(newptr);
}

/*  
 * Insert a block at the tail of the current block list
 */
CNblockptr CNinsert_block(block_listhead, block_listtail, 
                          cube,t000,t001,t010,t011,t100,t101,t110,t111,ID)
CNblockptr *block_listhead, *block_listtail;
CNcubeptr  cube;
double     t000,t001,t010,t011,t100,t101,t110,t111;
int        ID;
{
   CNblockptr CNmake_block();
   CNblockptr next,A,B;

   A = *block_listtail;
   if ((B=CNmake_block(cube,t000,t001,t010,t011,t100,t101,t110,t111,ID))!=NULL){
      if (A==NULL) {
         *block_listhead = B;
         *block_listtail = B;
      } else {
         next = A->next;
         B->next = next;
         B->prev = A;
         A->next = B;
         if (next != NULL) next->prev = B;
         if (B->next == NULL) *block_listtail = B;
      }
   }
   return(B);
}


/*
 * Delete block at address L 
 */
void CNdelete_block(block_listhead, block_listtail, L)
CNblockptr *block_listhead, *block_listtail;
CNblockptr L;
{
   CNblockptr prev,next;

   prev = L->prev;
   next = L->next;
   if (prev!=NULL) prev->next = next;
   if (next!=NULL) next->prev = prev;
   if (L== *block_listhead) *block_listhead = next;
   if (L== *block_listtail) *block_listtail = prev;
   free ((char*)L);
   L = NULL;
}


/* 
 * Delete all the blocks in the list
 */
void CNdelete_block_list(block_listhead, block_listtail)
CNblockptr *block_listhead, *block_listtail;
{
   CNblockptr B;

   while ((B = *block_listhead) != NULL)
      CNdelete_block(block_listhead, block_listtail, B);
}


/* 
 * Print out the list of blocks 
 */
/*ARGSUSED*/
void CNprint_block_list(block_listhead, block_listtail)
CNblockptr block_listhead, block_listtail;
{
   CNblockptr B;

   for (B=block_listhead; B!=NULL; B=B->next) 
      print_block(B);
}


/* 
 * print the coordinates of a block
 */
static void print_block(B)
CNblockptr B;
{
   (void) fprintf(stdout,"blockID=%d cubeID=%d\n",B->ID,B->cube->ID);
   (void) fprintf(stdout,"   t000=%8.5g t001=%8.5g\n",B->t000, B->t001);
   (void) fprintf(stdout,"   t010=%8.5g t011=%8.5g\n",B->t010, B->t011);
   (void) fprintf(stdout,"   t100=%8.5g t101=%8.5g\n",B->t100, B->t101);
   (void) fprintf(stdout,"   t110=%8.5g t111=%8.5g\n",B->t110, B->t111);
}


/*
 * Count the number of blocks in the list 
 */
/*ARGSUSED*/
int CNcount_blocks(block_listhead, block_listtail)
CNblockptr block_listhead, block_listtail;
{
   CNblockptr B;
   int       count = 0;

   for (B=block_listhead; B!=NULL; B=B->next) count++;

   return(count);
}

