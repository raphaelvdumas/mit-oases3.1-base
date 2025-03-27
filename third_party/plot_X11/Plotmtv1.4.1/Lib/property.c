/*
 * property.c - procedures for setting properties of curves, datasets and
 *              plotsets.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <malloc.h>
#include "CNdata.h"
#include "CNproperty.h"
#include "CNspline.h"
#include "CNstring.h"
#include "CNplot3D.h"


/*
 * Properties or data-options associated to a plot-set
 */

/*
 * Allocate room for a plotset property structure
 */
CNplotset_property *CNmake_plotset_property()
{
   CNplotset_property *newptr;
   unsigned int size = sizeof(CNplotset_property);
 
   if ((newptr = (CNplotset_property *)malloc(size))!=NULL) {
      CNset_default_plotset_property(newptr);
   }
   return(newptr);
}


/*
 * Delete a plotset property structure
 */
void CNdelete_plotset_property(prop)
CNplotset_property *prop;
{
   if (prop!=NULL) {
      if (prop->xlabel  ) free((char *)prop->xlabel);
      if (prop->ylabel  ) free((char *)prop->ylabel);
      if (prop->zlabel  ) free((char *)prop->zlabel);
      if (prop->toplabel) free((char *)prop->toplabel);
      if (prop->comment ) free((char *)prop->comment );
      if (prop->subtitle) free((char *)prop->subtitle);
      if (prop->xlabelhead)
         CNdelete_axislabel_list(&(prop->xlabelhead), &(prop->xlabeltail));
      if (prop->ylabelhead)
         CNdelete_axislabel_list(&(prop->ylabelhead), &(prop->ylabeltail));
      if (prop->zlabelhead)
         CNdelete_axislabel_list(&(prop->zlabelhead), &(prop->zlabeltail));
      free((char *)prop);
   }
}

/*
 * Delete the fields in a plotset property structure
 */
void CNdelete_plotset_property_fields(prop)
CNplotset_property *prop;
{
   if (prop!=NULL) {
      if (prop->xlabel  ) free((char *)prop->xlabel);
      if (prop->ylabel  ) free((char *)prop->ylabel);
      if (prop->zlabel  ) free((char *)prop->zlabel);
      if (prop->toplabel) free((char *)prop->toplabel);
      if (prop->comment ) free((char *)prop->comment );
      if (prop->subtitle) free((char *)prop->subtitle);
      if (prop->xlabelhead)
         CNdelete_axislabel_list(&(prop->xlabelhead), &(prop->xlabeltail));
      if (prop->ylabelhead)
         CNdelete_axislabel_list(&(prop->ylabelhead), &(prop->ylabeltail));
      if (prop->zlabelhead)
         CNdelete_axislabel_list(&(prop->zlabelhead), &(prop->zlabeltail));
   }
}


/*
 * Set the default property of a plotset
 */
void CNset_default_plotset_property(prop)
CNplotset_property *prop;
{
   /* Flag */
   prop->flag1     = 0;
   prop->flag2     = 0;

   /* Plot labels */
   prop->xlabel   = (char *)NULL;
   prop->ylabel   = (char *)NULL;
   prop->zlabel   = (char *)NULL;
   prop->toplabel = (char *)NULL;
   prop->comment  = (char *)NULL;
   prop->subtitle = (char *)NULL;

   /* Plotting parameters/options */
   prop->grid         = CN_FALSE;
   prop->xflip        = CN_FALSE;
   prop->yflip        = CN_FALSE;
   prop->xabs         = CN_FALSE;
   prop->yabs         = CN_FALSE;
   prop->zabs         = CN_FALSE;
   prop->xlog         = CN_FALSE;
   prop->ylog         = CN_FALSE;
   prop->zlog         = CN_FALSE;
   prop->xticks       = 4;
   prop->yticks       = 4;
   prop->zticks       = 4;
   prop->xautorange   = CN_TRUE;
   prop->yautorange   = CN_TRUE;
   prop->zautorange   = CN_TRUE;
   prop->equalscale   = CN_FALSE;
   prop->fitpage      = CN_TRUE;
   prop->xyratio      = 0.75;
   prop->xscale       = 1.00;
   prop->yscale       = 1.00;
   prop->zscale       = 1.00;
   prop->overlay      = CN_TRUE;
   prop->sidelabel    = CN_TRUE;
   prop->slabellen    = 0;
   prop->innerticks   = CN_FALSE;

   /* Viewport */
   prop->vxmin        = 0.00;
   prop->vxmax        = 1.00;
   prop->vymin        = 0.00;
   prop->vymax        = 1.00;
   prop->vzmin        = 0.00;
   prop->vzmax        = 1.00;

   /* Viewport */
   prop->prev_vxmin   = 0.00;
   prop->prev_vxmax   = 0.00;
   prop->prev_vymin   = 0.00;
   prop->prev_vymax   = 0.00;
   prop->prev_vzmin   = 0.00;
   prop->prev_vzmax   = 0.00;

   /* Plot Viewport */
   prop->pxmin        = 0.00;
   prop->pxmax        = 1.00;
   prop->pymin        = 0.00;
   prop->pymax        = 1.00;
   prop->pzmin        = 0.00;
   prop->pzmax        = 1.00;

   /* Linked lists for axis labels */
   prop->xlabelhead   = NULL;
   prop->xlabeltail   = NULL;
   prop->ylabelhead   = NULL;
   prop->ylabeltail   = NULL;
   prop->zlabelhead   = NULL;
   prop->zlabeltail   = NULL;
}


/*
 * Print the values of a plotset property, primarily for debugging purposes
 */
void CNprint_plotset_property(prop)
CNplotset_property *prop;
{
   (void) fprintf(stdout,"xlabel    = %s\n",prop->xlabel   );
   (void) fprintf(stdout,"ylabel    = %s\n",prop->ylabel   );
   (void) fprintf(stdout,"zlabel    = %s\n",prop->zlabel   );
   (void) fprintf(stdout,"toplabel  = %s\n",prop->toplabel );
   (void) fprintf(stdout,"comment   = %s\n",prop->comment  );
   (void) fprintf(stdout,"subtitle  = %s\n",prop->subtitle );
   (void) fprintf(stdout,"grid      = %s\n",BOOLEAN_VALUE(prop->grid     ));
   (void) fprintf(stdout,"xflip     = %s\n",BOOLEAN_VALUE(prop->xflip    ));
   (void) fprintf(stdout,"yflip     = %s\n",BOOLEAN_VALUE(prop->yflip    ));
   (void) fprintf(stdout,"xabs      = %s\n",BOOLEAN_VALUE(prop->xabs     ));
   (void) fprintf(stdout,"yabs      = %s\n",BOOLEAN_VALUE(prop->yabs     ));
   (void) fprintf(stdout,"zabs      = %s\n",BOOLEAN_VALUE(prop->zabs     ));
   (void) fprintf(stdout,"xlog      = %s\n",BOOLEAN_VALUE(prop->xlog     ));
   (void) fprintf(stdout,"ylog      = %s\n",BOOLEAN_VALUE(prop->ylog     ));
   (void) fprintf(stdout,"zlog      = %s\n",BOOLEAN_VALUE(prop->zlog     ));
   (void) fprintf(stdout,"xticks    = %d\n",prop->xticks   );
   (void) fprintf(stdout,"yticks    = %d\n",prop->yticks   );
   (void) fprintf(stdout,"zticks    = %d\n",prop->zticks   );
   (void) fprintf(stdout,"xautorange= %s\n",BOOLEAN_VALUE(prop->xautorange));
   (void) fprintf(stdout,"yautorange= %s\n",BOOLEAN_VALUE(prop->yautorange));
   (void) fprintf(stdout,"zautorange= %s\n",BOOLEAN_VALUE(prop->zautorange));
   (void) fprintf(stdout,"equalscale= %s\n",BOOLEAN_VALUE(prop->equalscale));
   (void) fprintf(stdout,"fitpage   = %s\n",BOOLEAN_VALUE(prop->fitpage   ));
   (void) fprintf(stdout,"xyratio   = %f\n",prop->xyratio );
   (void) fprintf(stdout,"xscale    = %g\n",prop->xscale  );
   (void) fprintf(stdout,"yscale    = %g\n",prop->yscale  );
   (void) fprintf(stdout,"zscale    = %g\n",prop->zscale  );
   (void) fprintf(stdout,"overlay   = %s\n",BOOLEAN_VALUE(prop->overlay   ));
   (void) fprintf(stdout,"sidelabel = %s\n",BOOLEAN_VALUE(prop->sidelabel ));
   (void) fprintf(stdout,"slabellen = %d\n",prop->slabellen);
   (void) fprintf(stdout,"innerticks= %s\n",BOOLEAN_VALUE(prop->innerticks));
   (void) fprintf(stdout,"vxmin     = %g\n",prop->vxmin   );
   (void) fprintf(stdout,"vxmax     = %g\n",prop->vxmax   );
   (void) fprintf(stdout,"vymin     = %g\n",prop->vymin   );
   (void) fprintf(stdout,"vymax     = %g\n",prop->vymax   );
   (void) fprintf(stdout,"vzmin     = %g\n",prop->vzmin   );
   (void) fprintf(stdout,"vzmax     = %g\n",prop->vzmax   );
   (void) fprintf(stdout,"pxmin     = %g\n",prop->pxmin   );
   (void) fprintf(stdout,"pxmax     = %g\n",prop->pxmax   );
   (void) fprintf(stdout,"pymin     = %g\n",prop->pymin   );
   (void) fprintf(stdout,"pymax     = %g\n",prop->pymax   );
   (void) fprintf(stdout,"pzmin     = %g\n",prop->pzmin   );
   (void) fprintf(stdout,"pzmax     = %g\n",prop->pzmax   );

   if (prop->xlabelhead != NULL) {
      (void) fprintf(stdout,"X-Axis labels:");
      CNprint_axislabel_list(prop->xlabelhead, prop->xlabeltail);
   }
   if (prop->ylabelhead != NULL) {
      (void) fprintf(stdout,"Y-Axis labels:");
      CNprint_axislabel_list(prop->ylabelhead, prop->ylabeltail);
   }
   if (prop->zlabelhead != NULL) {
      (void) fprintf(stdout,"Z-Axis labels:");
      CNprint_axislabel_list(prop->zlabelhead, prop->zlabeltail);
   }
}


/*
 * Copy the titles of a plotset 
 *    This is done by copying the contents of one plotset to another
 *    based on one plotset's flag.
 */
void CNset_plotset_titles(P1,P2)
CNplotset_property *P1, *P2;
{
   /* Error checking */
   if (P1==NULL || P2==NULL) {
      (void) fprintf(stderr,
      "Error in CNset_plotset_titles() : Null plotset property!\n");
      return;
   }
 
   /* Check the flag */
   if ((P2->flag1 & CNtitle     ) == 0) return;

   /* Character strings */
   if (P2->xlabel != NULL) {
      CNdestroy_string(P1->xlabel);
      P1->xlabel  = CNcreate_string(P2->xlabel);
   }
   if (P2->ylabel != NULL) {
      CNdestroy_string(P1->ylabel);
      P1->ylabel = CNcreate_string(P2->ylabel);
   }
   if (P2->zlabel != NULL) {
      CNdestroy_string(P1->zlabel);
      P1->zlabel = CNcreate_string(P2->zlabel);
   }
   if (P2->toplabel != NULL) {
      CNdestroy_string(P1->toplabel);
      P1->toplabel=CNcreate_string(P2->toplabel);
   }
   if (P2->comment != NULL) {
      if (P1->comment) CNdestroy_string(P1->comment);
      P1->comment = CNcreate_string(P2->comment);
   }
   if (P2->subtitle != NULL) {
      if (P1->subtitle) CNdestroy_string(P1->subtitle);
      P1->subtitle = CNcreate_string(P2->subtitle);
   }
}

/*
 * Set the property of a plotset
 *    This is done by copying the contents of one plotset to another
 *    based on one plotset's flag.
 */
void CNset_plotset_property(P1,P2)
CNplotset_property *P1, *P2;
{
   /* Error checking */
   if (P1==NULL || P2==NULL) {
      (void) fprintf(stderr,
      "Error in CNset_plotset_property() : Null plotset property!\n");
      return;
   }

   /* Set the flag */
   P1->flag1 = P1->flag1 | P2->flag1;
   P1->flag2 = P1->flag2 | P2->flag2;
 
   /* Set the plotset titles */
   CNset_plotset_titles(P1,P2);

   /* Normal variables */
   if ((P2->flag1 & CNgrid      ) != 0) P1->grid      = P2->grid;
   if ((P2->flag1 & CNxflip     ) != 0) P1->xflip     = P2->xflip;
   if ((P2->flag1 & CNyflip     ) != 0) P1->yflip     = P2->yflip;
   if ((P2->flag1 & CNxabs      ) != 0) P1->xabs      = P2->xabs;
   if ((P2->flag1 & CNyabs      ) != 0) P1->yabs      = P2->yabs;
   if ((P2->flag1 & CNzabs      ) != 0) P1->zabs      = P2->zabs;
   if ((P2->flag1 & CNxlog      ) != 0) P1->xlog      = P2->xlog;
   if ((P2->flag1 & CNylog      ) != 0) P1->ylog      = P2->ylog;
   if ((P2->flag1 & CNzlog      ) != 0) P1->zlog      = P2->zlog;
   if ((P2->flag1 & CNxticks    ) != 0) P1->xticks    = P2->xticks;
   if ((P2->flag1 & CNyticks    ) != 0) P1->yticks    = P2->yticks;
   if ((P2->flag1 & CNzticks    ) != 0) P1->zticks    = P2->zticks;
   if ((P2->flag1 & CNxautorange) != 0) P1->xautorange= P2->xautorange;
   if ((P2->flag1 & CNyautorange) != 0) P1->yautorange= P2->yautorange;
   if ((P2->flag1 & CNzautorange) != 0) P1->zautorange= P2->zautorange;
   if ((P2->flag1 & CNequalscale) != 0) P1->equalscale= P2->equalscale;
   if ((P2->flag1 & CNfitpage   ) != 0) P1->fitpage   = P2->fitpage;
   if ((P2->flag1 & CNxyratio   ) != 0) P1->xyratio   = P2->xyratio;
   if ((P2->flag1 & CNxscale    ) != 0) P1->xscale    = P2->xscale;
   if ((P2->flag1 & CNyscale    ) != 0) P1->yscale    = P2->yscale;
   if ((P2->flag1 & CNzscale    ) != 0) P1->zscale    = P2->zscale;
   if ((P2->flag1 & CNoverlay   ) != 0) P1->overlay   = P2->overlay;
   if ((P2->flag1 & CNvxmin     ) != 0) P1->vxmin     = P2->vxmin;
   if ((P2->flag1 & CNvxmax     ) != 0) P1->vxmax     = P2->vxmax;
   if ((P2->flag1 & CNvymin     ) != 0) P1->vymin     = P2->vymin;
   if ((P2->flag1 & CNvymax     ) != 0) P1->vymax     = P2->vymax;
   if ((P2->flag1 & CNvzmin     ) != 0) P1->vzmin     = P2->vzmin;
   if ((P2->flag1 & CNvzmax     ) != 0) P1->vzmax     = P2->vzmax;

   if ((P2->flag2 & CNsidelabel ) != 0) P1->sidelabel = P2->sidelabel;
   if ((P2->flag2 & CNslabellen ) != 0) P1->slabellen = P2->slabellen;
   if ((P2->flag2 & CNinnerticks) != 0) P1->innerticks= P2->innerticks;

   P1->pxmin   = P2->pxmin;
   P1->pxmax   = P2->pxmax;
   P1->pymin   = P2->pymin;
   P1->pymax   = P2->pymax;
   P1->pzmin   = P2->pzmin;
   P1->pzmax   = P2->pzmax;

   /* Copy over the axis label lists */
   if ((P2->flag2 & CNsetaxislbl) != 0) {
      CNcopy_axislabel_list(&(P1->xlabelhead), &(P1->xlabeltail),
                              P2->xlabelhead,    P2->xlabeltail);
      CNcopy_axislabel_list(&(P1->ylabelhead), &(P1->ylabeltail),
                              P2->ylabelhead,    P2->ylabeltail);
      CNcopy_axislabel_list(&(P1->zlabelhead), &(P1->zlabeltail),
                              P2->zlabelhead,    P2->zlabeltail);
   }
}

static enum plotset_keyvals {
   PLxlabel,
   PLylabel,
   PLzlabel,
   PLtlabel,
   PLcomment,
   PLsubtitle,
   PLxflip,
   PLyflip,
   PLxabs,
   PLyabs,
   PLzabs,
   PLxlog,
   PLylog,
   PLzlog,
   PLxticks,
   PLyticks,
   PLzticks,
   PLxautor,
   PLyautor,
   PLzautor,
   PLautor,
   PLgrid,
   PLeqscale,
   PLfitpage,
   PLxyratio,
   PLxscale,
   PLyscale,
   PLzscale,
   PLoverlay,
   PLsidelbl,
   PLslbllen,
   PLvxmin,
   PLvxmax,
   PLvymin,
   PLvymax,
   PLvzmin,
   PLvzmax,
   PLxaxislbl,
   PLyaxislbl,
   PLzaxislbl,
   PLinnertck
};

#define PLOTSET_MAXKEY 49
static CNkeyword plotset_keywords[] = {
   {"xlabel",    PLxlabel,  CN_STRING , 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"ylabel",    PLylabel,  CN_STRING , 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"zlabel",    PLzlabel,  CN_STRING , 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"toplabel",  PLtlabel,  CN_STRING , 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"comment",   PLcomment, CN_STRING , 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"subtitle",  PLsubtitle,CN_STRING , 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"xflip",     PLxflip,   CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"yflip",     PLyflip,   CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"xabs",      PLxabs,    CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"yabs",      PLyabs,    CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"zabs",      PLzabs,    CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"xlog",      PLxlog,    CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"ylog",      PLylog,    CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"zlog",      PLzlog,    CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"xticks",    PLxticks,  CN_INTEGER, 1, 1, 4, 1,50, 0.0, 0.0, 0.0},
   {"yticks",    PLyticks,  CN_INTEGER, 1, 1, 4, 1,50, 0.0, 0.0, 0.0},
   {"zticks",    PLzticks,  CN_INTEGER, 1, 1, 4, 1,50, 0.0, 0.0, 0.0},
   {"xautorange",PLxautor,  CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"yautorange",PLyautor,  CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"zautorange",PLzautor,  CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"autorange", PLautor,   CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"grid",      PLgrid,    CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"equalscale",PLeqscale, CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"fitpage",   PLfitpage, CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"xyratio",   PLxyratio, CN_DOUBLE,  1, 1, 0, 0, 0,0.75, 0.01,4.0},
   {"xscale",    PLxscale,  CN_DOUBLE,  1, 0, 0, 0, 0,1.00,1e-99,0.0},
   {"yscale",    PLyscale,  CN_DOUBLE,  1, 0, 0, 0, 0,1.00,1e-99,0.0},
   {"zscale",    PLzscale,  CN_DOUBLE,  1, 0, 0, 0, 0,1.00,1e-99,0.0},
   {"overlay",   PLoverlay, CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"sidelabel", PLsidelbl, CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"sidelabellength", PLslbllen, CN_INTEGER, 1, 1, 0, -100,300, 0.0, 0.0, 0.0},
   {"labeloffset",     PLslbllen, CN_INTEGER, 1, 1, 0, -100,300, 0.0, 0.0, 0.0},
   {"vxmin",     PLvxmin,   CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"vxmax",     PLvxmax,   CN_DOUBLE,  0, 0, 0, 0, 0, 1.0, 0.0, 0.0},
   {"vymin",     PLvymin,   CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"vymax",     PLvymax,   CN_DOUBLE,  0, 0, 0, 0, 0, 1.0, 0.0, 0.0},
   {"vzmin",     PLvzmin,   CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"vzmax",     PLvzmax,   CN_DOUBLE,  0, 0, 0, 0, 0, 1.0, 0.0, 0.0},
   {"xmin",      PLvxmin,   CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"xmax",      PLvxmax,   CN_DOUBLE,  0, 0, 0, 0, 0, 1.0, 0.0, 0.0},
   {"ymin",      PLvymin,   CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"ymax",      PLvymax,   CN_DOUBLE,  0, 0, 0, 0, 0, 1.0, 0.0, 0.0},
   {"zmin",      PLvzmin,   CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"zmax",      PLvzmax,   CN_DOUBLE,  0, 0, 0, 0, 0, 1.0, 0.0, 0.0},
   {"xticklabel",PLxaxislbl,CN_STRING , 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"yticklabel",PLyaxislbl,CN_STRING , 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"zticklabel",PLzaxislbl,CN_STRING , 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"innertick", PLinnertck,CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"innerticks",PLinnertck,CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
};

/*
 * Act on a plotset-keyword (argument=value) and change the plotset
 */
int CNparse_plotset_property(property,argument,value,verbose)
CNplotset_property  *property;           /* plotset property */
char                *argument,*value;
int                 verbose;
{
   CNkeyword *key;
   int       match = 0, keyval = -1, i;
   char      cval[CN_MAXCHAR];
   short     sval=0;
   int       ival=0;
   double    dval=0.0;

   /* Search for a matching argument keyword */
   for (i=0; i<PLOTSET_MAXKEY && !match; i++) {
      if (strcmp(plotset_keywords[i].keyword,argument) == 0) {
         match = 1;
         keyval = plotset_keywords[i].keyval;
         key    = &(plotset_keywords[i]);
      }
   }

   /* Return if not found */
   if (match == 0) return(match);

   /* Now get the corresponding value and change the structure */
   switch (keyval) {
   case PLxlabel :
      CNassign_string_keyword(cval,value,argument,verbose);
      property->flag1 = property->flag1 | CNtitle;
      CNdestroy_string(property->xlabel);
      property->xlabel  = CNcreate_string(cval);
      break;
   case PLylabel :
      CNassign_string_keyword(cval,value,argument,verbose);
      property->flag1 = property->flag1 | CNtitle;
      CNdestroy_string(property->ylabel);
      property->ylabel  = CNcreate_string(cval);
      break;
   case PLzlabel :
      CNassign_string_keyword(cval,value,argument,verbose);
      property->flag1 = property->flag1 | CNtitle;
      CNdestroy_string(property->zlabel);
      property->zlabel  = CNcreate_string(cval);
      break;
   case PLtlabel :
      CNassign_string_keyword(cval,value,argument,verbose);
      property->flag1 = property->flag1 | CNtitle;
      CNdestroy_string(property->toplabel);
      property->toplabel  = CNcreate_string(cval);
      break;
   case PLcomment :
      CNassign_string_keyword(cval,value,argument,verbose);
      property->flag1 = property->flag1 | CNtitle;
      CNdestroy_string(property->comment);
      property->comment   = CNcreate_string(cval);
      break;
   case PLsubtitle:
      CNassign_string_keyword(cval,value,argument,verbose);
      property->flag1 = property->flag1 | CNtitle;
      CNdestroy_string(property->subtitle);
      property->subtitle  = CNcreate_string(cval);
      break;
   case PLxflip  :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNxflip;
      property->xflip = sval;
      break;
   case PLyflip  :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNyflip;
      property->yflip = sval;
      break;
   case PLxabs  :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNxabs;
      property->xabs = sval;
      break;
   case PLyabs  :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNyabs;
      property->yabs = sval;
      break;
   case PLzabs  :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNzabs;
      property->zabs = sval;
      break;
   case PLxlog  :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNxlog;
      property->xlog = sval;
      break;
   case PLylog  :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNylog;
      property->ylog = sval;
      break;
   case PLzlog  :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNzlog;
      property->zlog = sval;
      break;
   case PLxautor  :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNxautorange;
      property->xautorange = sval;
      break;
   case PLyautor  :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNyautorange;
      property->yautorange = sval;
      break;
   case PLzautor  :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNzautorange;
      property->zautorange = sval;
      break;
   case PLautor :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNxautorange;
      property->flag1 = property->flag1 | CNyautorange;
      property->flag1 = property->flag1 | CNzautorange;
      property->xautorange = sval;
      property->yautorange = sval;
      property->zautorange = sval;
      break;
   case PLxticks :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      property->flag1 = property->flag1 | CNxticks;
      property->xticks= ival;
      property->flag1 = property->flag1 | CNxautorange;
      property->xautorange = CN_FALSE;
      break;
   case PLyticks :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      property->flag1 = property->flag1 | CNyticks;
      property->yticks= ival;
      property->flag1 = property->flag1 | CNyautorange;
      property->yautorange = CN_FALSE;
      break;
   case PLzticks :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      property->flag1 = property->flag1 | CNzticks;
      property->zticks= ival;
      property->flag1 = property->flag1 | CNzautorange;
      property->zautorange = CN_FALSE;
      break;
   case PLgrid  :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNgrid;
      property->grid = sval;
      break;
   case PLeqscale :
      CNassign_boolean_keyword(&sval,value,argument,verbose);
      property->flag1 = property->flag1 | CNequalscale;
      property->flag1 = property->flag1 | CNfitpage;
      property->equalscale  = sval;
      property->fitpage     = !sval;
      break;
   case PLfitpage :
      CNassign_boolean_keyword(&sval,value,argument,verbose);
      property->flag1 = property->flag1 | CNequalscale;
      property->flag1 = property->flag1 | CNfitpage;
      property->equalscale  = !sval;
      property->fitpage     = sval;
      break;
   case PLxyratio :
      CNassign_double_keyword(&dval,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      property->flag1 = property->flag1 | CNequalscale;
      property->flag1 = property->flag1 | CNfitpage;
      property->flag1 = property->flag1 | CNxyratio;
      property->xyratio    = dval;
      property->equalscale = CN_FALSE;
      property->fitpage    = CN_FALSE;
      break;
   case PLxscale :
      CNassign_double_keyword(&dval,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      property->flag1   = property->flag1 | CNxscale;
      property->xscale = dval;
      break;
   case PLyscale :
      CNassign_double_keyword(&dval,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      property->flag1   = property->flag1 | CNyscale;
      property->yscale = dval;
      break;
   case PLzscale :
      CNassign_double_keyword(&dval,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      property->flag1   = property->flag1 | CNzscale;
      property->zscale = dval;
      break;
   case PLoverlay :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1    = property->flag1 | CNoverlay;
      property->overlay = sval;
      break;
   case PLsidelbl :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag2    = property->flag2 | CNsidelabel;
      property->sidelabel = sval;
      break;
   case PLslbllen :
      CNassign_int_keyword(&ival ,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      property->flag2    = property->flag2 | CNslabellen;
      property->slabellen = ival;
      break;
   case PLvxmin :
      CNassign_double_keyword(&dval,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      property->flag1  = property->flag1 | CNvxmin;
      property->vxmin = dval;
      break;
   case PLvxmax :
      CNassign_double_keyword(&dval,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      property->flag1 = property->flag1 | CNvxmax;
      property->vxmax = dval;
      break;
   case PLvymin :
      CNassign_double_keyword(&dval,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      property->flag1 = property->flag1 | CNvymin;
      property->vymin = dval;
      break;
   case PLvymax :
      CNassign_double_keyword(&dval,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      property->flag1 = property->flag1 | CNvymax;
      property->vymax = dval;
      break;
   case PLvzmin :
      CNassign_double_keyword(&dval,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      property->flag1 = property->flag1 | CNvzmin;
      property->vzmin = dval;
      break;
   case PLvzmax :
      CNassign_double_keyword(&dval,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      property->flag1 = property->flag1 | CNvzmax;
      property->vzmax = dval;
      break;

   case PLxaxislbl :
      CNparse_axislabel_string(&(property->xlabelhead), 
                               &(property->xlabeltail),
                               value);
      property->flag2 = property->flag2 | CNsetaxislbl;
      break;
   case PLyaxislbl :
      CNparse_axislabel_string(&(property->ylabelhead), 
                               &(property->ylabeltail),
                               value);
      property->flag2 = property->flag2 | CNsetaxislbl;
      break;
   case PLzaxislbl :
      CNparse_axislabel_string(&(property->zlabelhead), 
                               &(property->zlabeltail),
                               value);
      property->flag2 = property->flag2 | CNsetaxislbl;
      break;
   case PLinnertck:
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag2    = property->flag2 | CNinnerticks;
      property->innerticks= sval;
      break;
   default :
      match = 0;
      break;
   }

   /* Return status */
   return(match);
}


/*
 * Print the list of allowable keywords
 */
void CNprint_plotset_keywords(verbose)
int verbose;
{
   CNprint_keywords(plotset_keywords,PLOTSET_MAXKEY,"Plotset",verbose);
}


/*
 * Print the list of keywords that have been set 
 */
void CNwrite_plotset_options(fp,prop)
FILE *fp;
CNplotset_property *prop;
{
   CNplotset_property def_prop;

   /* Set default properties */
   CNset_default_plotset_property(&def_prop);

   /* Print out each field if it differs from the default value */
   if (prop->xlabel)
   (void) fprintf(fp,"%% xlabel    = \"%s\"\n",prop->xlabel   );

   if (prop->ylabel)
   (void) fprintf(fp,"%% ylabel    = \"%s\"\n",prop->ylabel   );

   if (prop->zlabel)
   (void) fprintf(fp,"%% zlabel    = \"%s\"\n",prop->zlabel   );

   if (prop->toplabel)
   (void) fprintf(fp,"%% toplabel  = \"%s\"\n",prop->toplabel );

   if (prop->comment)
   (void) fprintf(fp,"%% comment   = \"%s\"\n",prop->comment  );

   if (prop->subtitle)
   (void) fprintf(fp,"%% subtitle  = \"%s\"\n",prop->subtitle );

   if (prop->grid != def_prop.grid)
   (void) fprintf(fp,"%% grid      = %s\n",BOOLEAN_VALUE(prop->grid     ));

   if (prop->xflip != def_prop.xflip)
   (void) fprintf(fp,"%% xflip     = %s\n",BOOLEAN_VALUE(prop->xflip    ));

   if (prop->yflip != def_prop.yflip)
   (void) fprintf(fp,"%% yflip     = %s\n",BOOLEAN_VALUE(prop->yflip    ));

   if (prop->xabs != def_prop.xabs) 
   (void) fprintf(fp,"%% xabs      = %s\n",BOOLEAN_VALUE(prop->xabs     ));

   if (prop->yabs != def_prop.yabs) 
   (void) fprintf(fp,"%% yabs      = %s\n",BOOLEAN_VALUE(prop->yabs     ));

   if (prop->zabs != def_prop.zabs) 
   (void) fprintf(fp,"%% zabs      = %s\n",BOOLEAN_VALUE(prop->zabs     ));

   if (prop->xlog != def_prop.xlog) 
   (void) fprintf(fp,"%% xlog      = %s\n",BOOLEAN_VALUE(prop->xlog     ));

   if (prop->ylog != def_prop.ylog) 
   (void) fprintf(fp,"%% ylog      = %s\n",BOOLEAN_VALUE(prop->ylog     ));

   if (prop->zlog != def_prop.zlog) 
   (void) fprintf(fp,"%% zlog      = %s\n",BOOLEAN_VALUE(prop->zlog     ));

   if (prop->xticks != def_prop.xticks) 
   (void) fprintf(fp,"%% xticks    = %d\n",prop->xticks   );

   if (prop->yticks != def_prop.yticks) 
   (void) fprintf(fp,"%% yticks    = %d\n",prop->yticks   );

   if (prop->zticks != def_prop.zticks) 
   (void) fprintf(fp,"%% zticks    = %d\n",prop->zticks   );

   if (prop->xautorange != def_prop.xautorange) 
   (void) fprintf(fp,"%% xautorange= %s\n",BOOLEAN_VALUE(prop->xautorange));

   if (prop->yautorange != def_prop.yautorange) 
   (void) fprintf(fp,"%% yautorange= %s\n",BOOLEAN_VALUE(prop->yautorange));

   if (prop->zautorange != def_prop.zautorange) 
   (void) fprintf(fp,"%% zautorange= %s\n",BOOLEAN_VALUE(prop->zautorange));

   if (prop->equalscale != def_prop.equalscale) 
   (void) fprintf(fp,"%% equalscale= %s\n",BOOLEAN_VALUE(prop->equalscale));
       
   if (prop->fitpage != def_prop.fitpage) 
   (void) fprintf(fp,"%% fitpage   = %s\n",BOOLEAN_VALUE(prop->fitpage   ));

   if (prop->xyratio != def_prop.xyratio) 
   (void) fprintf(fp,"%% xyratio   = %f\n",prop->xyratio );

   if (prop->xscale != def_prop.xscale) 
   (void) fprintf(fp,"%% xscale    = %g\n",prop->xscale  );

   if (prop->yscale != def_prop.yscale) 
   (void) fprintf(fp,"%% yscale    = %g\n",prop->yscale  );

   if (prop->zscale != def_prop.zscale) 
   (void) fprintf(fp,"%% zscale    = %g\n",prop->zscale  );

   if (prop->overlay != def_prop.overlay) 
   (void) fprintf(fp,"%% overlay   = %s\n",BOOLEAN_VALUE(prop->overlay));

   if (prop->sidelabel != def_prop.sidelabel) 
   (void) fprintf(fp,"%% sidelabel = %s\n",BOOLEAN_VALUE(prop->sidelabel));

   if (prop->slabellen != def_prop.slabellen) 
   (void) fprintf(fp,"%% sidelabellength = %d\n",prop->slabellen);

   if (prop->innerticks != def_prop.innerticks) 
   (void) fprintf(fp,"%% innerticks= %s\n",BOOLEAN_VALUE(prop->innerticks));

   /* Derived quantities - print only if the flag is set */
   if ((prop->flag1 & CNvxmin) != 0) 
   (void) fprintf(fp,"%% xmin      = %g\n",prop->vxmin);

   if ((prop->flag1 & CNvymin) != 0) 
   (void) fprintf(fp,"%% ymin      = %g\n",prop->vymin);

   if ((prop->flag1 & CNvzmin) != 0) 
   (void) fprintf(fp,"%% zmin      = %g\n",prop->vzmin);

   if ((prop->flag1 & CNvxmax) != 0) 
   (void) fprintf(fp,"%% xmax      = %g\n",prop->vxmax);

   if ((prop->flag1 & CNvymax) != 0) 
   (void) fprintf(fp,"%% ymax      = %g\n",prop->vymax);

   if ((prop->flag1 & CNvzmax) != 0) 
   (void) fprintf(fp,"%% zmax      = %g\n",prop->vzmax);

   if (prop->xlabelhead != NULL) 
      CNwrite_xaxislabels(fp, prop->xlabelhead, prop->xlabeltail);
   if (prop->ylabelhead != NULL) 
      CNwrite_yaxislabels(fp, prop->ylabelhead, prop->ylabeltail);
   if (prop->zlabelhead != NULL) 
      CNwrite_zaxislabels(fp, prop->zlabelhead, prop->zlabeltail);
}


/*
 * Properties or data-options associated to a data-set
 */

/*
 * Allocate room for a dataset property structure
 */
CNdataset_property *CNmake_dataset_property()
{
   CNdataset_property *newptr;
   unsigned int size = sizeof(CNdataset_property);
 
   if ((newptr = (CNdataset_property *)malloc(size))!=NULL) {
      CNset_default_dataset_property(newptr);
   }
   return(newptr);
}


/*
 * Delete a dataset property structure
 */
void CNdelete_dataset_property(prop)
CNdataset_property *prop;
{
   if (prop!=NULL) free((char *)prop);
}


/*
 * Delete the fields in a dataset property structure
 */
void CNdelete_dataset_property_fields(prop)
CNdataset_property *prop;
{
   if (prop!=NULL) {
      if (prop->contours) free((char *)prop->contours);
   }
}

/*
 * Set the default property of a dataset
 */
void CNset_default_dataset_property(prop)
CNdataset_property *prop;
{
   /* Flag */
   prop->flag1     = 0;
   prop->flag2     = 0;

   /* Contour-specific */
   prop->contstyle = CN_LINECONT;
   prop->stepmethod= CN_STEPSIZE;
   prop->contours  = (char *)NULL;
   prop->cstep     = 0.0;
   prop->nsteps    = CN_IDLSTEPS;
   prop->logzstep  = 1;
   prop->cmax      = 0.0;
   prop->cmin      = 0.0;
   prop->linetypes = 2;
   prop->logx      = CN_FALSE;     /* Used also for cutlines */
   prop->logy      = CN_FALSE;     /* Used also for cutlines */
   prop->logz      = CN_FALSE;     /* Used also for cutlines */
   prop->contlabel = CN_TRUE;
   prop->contintrp = CN_RECTRECT;
   prop->contclip  = CN_FALSE;
   prop->contbitmap= CN_FALSE;

   /* Related to mesh data */
   prop->meshplot  = CN_FALSE;
   prop->boundary  = CN_TRUE;
   prop->regbound  = CN_FALSE;
   prop->fillbnd   = CN_FALSE;

   /* Vectors */
   prop->vlog      = CN_FALSE;
   prop->vlogscale = 1.0;
   prop->vscale    = 1.0;
   prop->vhead     = CN_TRUE;
   prop->vtail     = CN_FALSE;

   /* Curve-related data */
   prop->applyfill = CN_TRUE;
   prop->splinetyp = CN_SP_NONE;

   /* Print element IDs */
   prop->pr_ptID   = CN_FALSE;
   prop->pr_ndID   = CN_FALSE;
   prop->pr_trID   = CN_FALSE;
   prop->pr_rtID   = CN_FALSE;
   prop->pr_rgID   = CN_FALSE;
   prop->pr_cvID   = CN_FALSE;

   /* Histogram */
   prop->binwidth  =  1.0;
   prop->binstart  =  0.0;

   /* Barchart */
   prop->barmin    =  0.0;

   /* Misc */
   prop->plotannot = CN_TRUE;
}


/*
 * Print the values of a dataset property, primarily for debugging purposes
 */
void CNprint_dataset_property(prop)
CNdataset_property *prop;
{
   (void) fprintf(stdout,"contstyle = %d\n",prop->contstyle);
   (void) fprintf(stdout,"step mtd  = %d\n",prop->stepmethod);
   (void) fprintf(stdout,"contours  = %s\n",prop->contours );
   (void) fprintf(stdout,"cstep     = %f\n",prop->cstep    );
   (void) fprintf(stdout,"nsteps    = %d\n",prop->nsteps   );
   (void) fprintf(stdout,"logzstep  = %d\n",prop->logzstep );
   (void) fprintf(stdout,"cmax      = %f\n",prop->cmax     );
   (void) fprintf(stdout,"cmin      = %f\n",prop->cmin     );
   (void) fprintf(stdout,"logx      = %s\n",BOOLEAN_VALUE(prop->logx     ));
   (void) fprintf(stdout,"logy      = %s\n",BOOLEAN_VALUE(prop->logy     ));
   (void) fprintf(stdout,"logz      = %s\n",BOOLEAN_VALUE(prop->logz     ));
   (void) fprintf(stdout,"linetypes = %d\n",prop->linetypes);
   (void) fprintf(stdout,"contlabel = %s\n",BOOLEAN_VALUE(prop->contlabel));
   (void) fprintf(stdout,"contintrp = %d\n",prop->contintrp);
   (void) fprintf(stdout,"contclip  = %s\n",BOOLEAN_VALUE(prop->contclip ));
   (void) fprintf(stdout,"contbitmap= %s\n",BOOLEAN_VALUE(prop->contbitmap));
   (void) fprintf(stdout,"meshplot  = %s\n",BOOLEAN_VALUE(prop->meshplot ));
   (void) fprintf(stdout,"boundary  = %s\n",BOOLEAN_VALUE(prop->boundary ));
   (void) fprintf(stdout,"regbound  = %s\n",BOOLEAN_VALUE(prop->regbound ));
   (void) fprintf(stdout,"fillbnd   = %s\n",BOOLEAN_VALUE(prop->fillbnd  ));
   (void) fprintf(stdout,"applyfill = %s\n",BOOLEAN_VALUE(prop->applyfill));
   (void) fprintf(stdout,"spline    = %s\n",CNsplinetype(prop->splinetyp));
   (void) fprintf(stdout,"vlog      = %s\n",BOOLEAN_VALUE(prop->vlog));
   (void) fprintf(stdout,"vlogscale = %g\n",prop->vlogscale);
   (void) fprintf(stdout,"vscale    = %g\n",prop->vscale);
   (void) fprintf(stdout,"vhead     = %s\n",BOOLEAN_VALUE(prop->vhead));
   (void) fprintf(stdout,"vtail     = %s\n",BOOLEAN_VALUE(prop->vtail));
   (void) fprintf(stdout,"pr_ptID   = %s\n",BOOLEAN_VALUE(prop->pr_ptID  ));
   (void) fprintf(stdout,"pr_ndID   = %s\n",BOOLEAN_VALUE(prop->pr_ndID  ));
   (void) fprintf(stdout,"pr_trID   = %s\n",BOOLEAN_VALUE(prop->pr_trID  ));
   (void) fprintf(stdout,"pr_rtID   = %s\n",BOOLEAN_VALUE(prop->pr_rtID  ));
   (void) fprintf(stdout,"pr_rgID   = %s\n",BOOLEAN_VALUE(prop->pr_rgID  ));
   (void) fprintf(stdout,"pr_cvID   = %s\n",BOOLEAN_VALUE(prop->pr_cvID  ));
   (void) fprintf(stdout,"binwidth  = %g\n",prop->binwidth);
   (void) fprintf(stdout,"binstart  = %g\n",prop->binstart);
   (void) fprintf(stdout,"barmin    = %g\n",prop->barmin  );
   (void) fprintf(stdout,"annotate  = %s\n",BOOLEAN_VALUE(prop->plotannot));
}


/*
 * Set the property of a dataset
 *    This is done by copying the contents of one dataset to another
 *    based on one dataset's flag.
 */
void CNset_dataset_property(D1,D2)
CNdataset_property *D1, *D2;
{
   /* Error checking */
   if (D1==NULL || D2==NULL) {
      (void) fprintf(stderr,
      "Error in CNset_dataset_property() : Null dataset property!\n");
      return;
   }

   D1->flag1 = D1->flag1 | D2->flag1;
   D1->flag2 = D1->flag2 | D2->flag2;

   /* Character strings */
   if ((D2->flag1 & CNctrlevel  ) != 0) {
      if (D2->contours != NULL) {
      CNdestroy_string(D1->contours);
      D1->contours  = CNcreate_string(D2->contours);
      }
   }

  if ((D2->flag1 & CNcontstyle ) != 0) D1->contstyle = D2->contstyle;
  if ((D2->flag1 & CNstepmethod) != 0) D1->stepmethod= D2->stepmethod;
  if ((D2->flag1 & CNcstep     ) != 0) D1->cstep     = D2->cstep;
  if ((D2->flag1 & CNnstep     ) != 0) D1->nsteps    = D2->nsteps;
  if ((D2->flag1 & CNlogzstep  ) != 0) D1->logzstep  = D2->logzstep;
  if ((D2->flag1 & CNcmax      ) != 0) D1->cmax      = D2->cmax ;
  if ((D2->flag1 & CNcmin      ) != 0) D1->cmin      = D2->cmin ;
  if ((D2->flag1 & CNlogx      ) != 0) D1->logx      = D2->logx ;
  if ((D2->flag1 & CNlogy      ) != 0) D1->logy      = D2->logy ;
  if ((D2->flag1 & CNlogz      ) != 0) D1->logz      = D2->logz ;
  if ((D2->flag1 & CNlinetypes ) != 0) D1->linetypes = D2->linetypes;
  if ((D2->flag1 & CNcontlabel ) != 0) D1->contlabel = D2->contlabel;
  if ((D2->flag1 & CNcontintrp ) != 0) D1->contintrp = D2->contintrp;
  if ((D2->flag1 & CNcontclip  ) != 0) D1->contclip  = D2->contclip ;
  if ((D2->flag1 & CNcontbitmap) != 0) D1->contbitmap= D2->contbitmap;
  if ((D2->flag1 & CNmeshplot  ) != 0) D1->meshplot  = D2->meshplot;
  if ((D2->flag1 & CNboundary  ) != 0) D1->boundary  = D2->boundary;
  if ((D2->flag1 & CNregbound  ) != 0) D1->regbound  = D2->regbound;
  if ((D2->flag1 & CNfillbnd   ) != 0) D1->fillbnd   = D2->fillbnd ;
  if ((D2->flag1 & CNapplyfill ) != 0) D1->applyfill = D2->applyfill;
  if ((D2->flag1 & CNsplinetyp ) != 0) D1->splinetyp = D2->splinetyp;
  if ((D2->flag1 & CNvlog      ) != 0) D1->vlog      = D2->vlog     ;
  if ((D2->flag1 & CNvlogscale ) != 0) D1->vlogscale = D2->vlogscale;
  if ((D2->flag1 & CNvscale    ) != 0) D1->vscale    = D2->vscale;
  if ((D2->flag1 & CNvhead     ) != 0) D1->vhead     = D2->vhead ;
  if ((D2->flag1 & CNvtail     ) != 0) D1->vtail     = D2->vtail ;

  if ((D2->flag2 & CNpr_ptID   ) != 0) D1->pr_ptID   = D2->pr_ptID ;
  if ((D2->flag2 & CNpr_ndID   ) != 0) D1->pr_ndID   = D2->pr_ndID ;
  if ((D2->flag2 & CNpr_trID   ) != 0) D1->pr_trID   = D2->pr_trID ;
  if ((D2->flag2 & CNpr_rtID   ) != 0) D1->pr_rtID   = D2->pr_rtID ;
  if ((D2->flag2 & CNpr_rgID   ) != 0) D1->pr_rgID   = D2->pr_rgID ;
  if ((D2->flag2 & CNpr_cvID   ) != 0) D1->pr_cvID   = D2->pr_cvID ;
  if ((D2->flag2 & CNbinwidth  ) != 0) D1->binwidth  = D2->binwidth;
  if ((D2->flag2 & CNbinstart  ) != 0) D1->binstart  = D2->binstart;
  if ((D2->flag2 & CNbarmin    ) != 0) D1->barmin    = D2->barmin  ;
  if ((D2->flag2 & CNplotannot ) != 0) D1->plotannot = D2->plotannot;
}

static enum dataset_keyvals {
   DScstep, 
   DSnsteps,
   DSlogzstep,
   DScontour,
   DScmax,
   DScmin,
   DSlogx,
   DSlogy,
   DSlogz,
   DSnlines,
   DScstyle,
   DScfill,
   DSclabel,
   DScintrp,
   DScclip,
   DSbitmap,
   DSpmesh,
   DSbound,
   DSregbound,
   DSfillbnd,
   DSvlog,
   DSvlogsc,
   DSvscale,
   DSvhead,
   DSvtail,
   DSspline,
   DSapfill,
   DSprintID,
   DSpr_ptID,
   DSpr_ndID,
   DSpr_trID,
   DSpr_rtID,
   DSpr_rgID,
   DSpr_cvID,
   DShbnwidth,
   DShbnstart,
   DSbarmin,
   DSplotann
};
#define DATASET_MAXKEY 55
static CNkeyword dataset_keywords[] = {
   {"stepsize",   DScstep,     CN_DOUBLE,  1, 0, 0, 0, 0, 0.1, 0.0, 0.0},
   {"cstep",      DScstep,     CN_DOUBLE,  1, 0, 0, 0, 0, 0.1, 0.0, 0.0},
   {"nsteps",     DSnsteps,    CN_INTEGER, 1, 1,10, 0,50, 0.0, 0.0, 0.0},
   {"logzsteps",  DSlogzstep,  CN_INTEGER, 1, 1, 1, 1,10, 0.0, 0.0, 0.0},
   {"logzstep",   DSlogzstep,  CN_INTEGER, 1, 1, 1, 1,10, 0.0, 0.0, 0.0},
   {"logsteps",   DSlogzstep,  CN_INTEGER, 1, 1, 1, 1,10, 0.0, 0.0, 0.0},
   {"logstep",    DSlogzstep,  CN_INTEGER, 1, 1, 1, 1,10, 0.0, 0.0, 0.0},
   {"contours",   DScontour,   CN_STRING,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"contmax",    DScmax,      CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"cmax",       DScmax,      CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"contmin",    DScmin,      CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"cmin",       DScmin,      CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"logx",       DSlogx,      CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"logy",       DSlogy,      CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"logz",       DSlogz,      CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"linetypes",  DSnlines,    CN_INTEGER, 1, 1, 2, 1, 3, 0.0, 0.0, 0.0},
   {"contstyle",  DScstyle,    CN_INTEGER, 1, 1, 1, 1, 4, 0.0, 0.0, 0.0},
   {"contfill",   DScfill,     CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"contlabel",  DSclabel,    CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"interpolate",DScintrp,    CN_INTEGER, 1, 1, 0, 0, 4, 0.0, 0.0, 0.0},
   {"interp",     DScintrp,    CN_INTEGER, 1, 1, 0, 0, 4, 0.0, 0.0, 0.0},
   {"continterp", DScintrp,    CN_INTEGER, 1, 1, 0, 0, 4, 0.0, 0.0, 0.0},
   {"contintrp",  DScintrp,    CN_INTEGER, 1, 1, 0, 0, 4, 0.0, 0.0, 0.0},
   {"contclip",   DScclip,     CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"bitmap",     DSbitmap,    CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"contbitmap", DSbitmap,    CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"meshplot",   DSpmesh,     CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"boundary",   DSbound,     CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"regbound",   DSregbound,  CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"fillbound",  DSfillbnd,   CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"fillbnd",    DSfillbnd,   CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"vlog",       DSvlog,      CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"vlogscale",  DSvlogsc,    CN_DOUBLE,  1, 0, 0, 0, 0, 1.0, 0.0, 0.0},
   {"vscale",     DSvscale,    CN_DOUBLE,  1, 0, 0, 0, 0, 1.0, 0.0, 0.0},
   {"vhead",      DSvhead,     CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"vtail",      DSvtail,     CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"spline",     DSspline,    CN_INTEGER, 1, 0, 1, 0, 6, 0.0, 0.0, 0.0},
   {"splinetype", DSspline,    CN_INTEGER, 1, 0, 1, 0, 6, 0.0, 0.0, 0.0},
   {"applyfill",  DSapfill,    CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"printid",    DSprintID,   CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"pointid",    DSpr_ptID,   CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"nodeid",     DSpr_ndID,   CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"triaid",     DSpr_trID,   CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"rectid",     DSpr_rtID,   CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"regionid",   DSpr_rgID,   CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"curveid",    DSpr_cvID,   CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"binwidth",   DShbnwidth,  CN_DOUBLE,  1, 0, 0, 0, 0, 1.0, 0.0, 0.0},
   {"bin_width",  DShbnwidth,  CN_DOUBLE,  1, 0, 0, 0, 0, 1.0, 0.0, 0.0},
   {"binstart",   DShbnstart,  CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"bin_start",  DShbnstart,  CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"bar",        DSbarmin,    CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"barmin",     DSbarmin,    CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"bar_min",    DSbarmin,    CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"annot",      DSplotann,   CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"annotate",   DSplotann,   CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
};

/*
 * Act on a dataset-keyword (argument=value) and change the dataset
 */
int CNparse_dataset_property(property,argument,value,verbose)
CNdataset_property  *property;           /* Dataset property */
char                *argument,*value;
int                 verbose;
{
   CNkeyword *key;
   int    match = 0, keyval = -1, i;
   short  sval=0;
   int    ival=0;
   double dval=0.0;
   char   cval[CN_MAXCHAR];

   /* Search for a matching argument keyword */
   for (i=0; i<DATASET_MAXKEY && !match; i++) {
      if (strcmp(dataset_keywords[i].keyword,argument) == 0) {
         match = 1;
         keyval = dataset_keywords[i].keyval;
         key    = &(dataset_keywords[i]);
      }
   }

   /* Return if not found */
   if (match == 0) return(match);

   /* Now get the corresponding value and change the structure */
   switch (keyval) {
   case DScstep :
      CNassign_double_keyword(&dval ,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      property->flag1  = property->flag1 | CNcstep;
      property->cstep  = dval;
      property->flag1  = property->flag1 | CNstepmethod;
      property->stepmethod = CN_STEPSIZE;
      break;
   case DSnsteps :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      property->flag1  = property->flag1 | CNnstep;
      property->nsteps = ival;
      property->flag1  = property->flag1 | CNstepmethod;
      property->stepmethod = CN_NUMSTEPS;
      break;
   case DSlogzstep :
      CNassign_int_keyword(&ival ,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      property->flag1 = property->flag1 | CNlogzstep;
      property->logzstep = ival;
      property->flag1  = property->flag1 | CNstepmethod;
      property->stepmethod = CN_LOGSTEPS;
      break;
   case DScontour :
      CNassign_string_keyword(cval,value,argument,verbose);
      property->flag1 = property->flag1 | CNctrlevel;
      CNdestroy_string(property->contours);
      property->contours  = CNcreate_string(cval);
      property->flag1  = property->flag1 | CNstepmethod;
      property->stepmethod = CN_USERDEFN;
      break;
   case DScmax :
      CNassign_double_keyword(&dval ,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      property->flag1 = property->flag1 | CNcmax;
      property->cmax = dval;
      break;
   case DScmin :
      CNassign_double_keyword(&dval ,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      property->flag1 = property->flag1 | CNcmin;
      property->cmin = dval;
      break;
   case DSlogx :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNlogx;
      property->logx = sval;
      break;
   case DSlogy :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNlogy;
      property->logy = sval;
      break;
   case DSlogz :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNlogz;
      property->logz = sval;
      break;
   case DSnlines :
      CNassign_int_keyword(&ival ,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      property->flag1 = property->flag1 | CNlinetypes;
      property->linetypes = ival;
      break;
   case DScstyle :
      CNassign_int_keyword(&ival ,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      property->flag1 = property->flag1 | CNcontstyle;
      property->contstyle = ival;
      break;
   case DScfill :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNcontstyle;
      property->contstyle = sval ? CN_FILLCONT : CN_LINECONT;
      break;
   case DSclabel :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNcontlabel;
      property->contlabel  = sval;
      break;
   case DScintrp :
      CNassign_int_keyword(&ival ,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      property->flag1 = property->flag1 | CNcontintrp;
      property->contintrp = ival;
      break;
   case DScclip  :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNcontclip;
      property->contclip   = sval;
      break;
   case DSbitmap :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNcontbitmap;
      property->contbitmap = sval;
      break;
   case DSpmesh :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNmeshplot;
      property->meshplot = sval;
      break;
   case DSbound :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNboundary;
      property->boundary = sval;
      break;
   case DSregbound :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNregbound;
      property->regbound = sval;
      break;
   case DSfillbnd:
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNfillbnd;
      property->fillbnd = sval;
      break;
   case DSvlog   :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNvlog;
      property->vlog = sval;
      break;
   case DSvlogsc :
      CNassign_double_keyword(&dval ,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      property->flag1 = property->flag1 | CNvlogscale;
      property->vlogscale = dval;
      break;
   case DSvscale :
      CNassign_double_keyword(&dval ,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      property->flag1 = property->flag1 | CNvscale;
      property->vscale = dval;
      break;
   case DSvhead  :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNvhead;
      property->vhead = sval;
      break;
   case DSvtail  :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag1 = property->flag1 | CNvtail;
      property->vtail = sval;
      break;
   case DSspline :
      CNassign_int_keyword(&ival ,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      if (verbose)
      (void) fprintf(stdout,"             Interpolation Method    = %s\n",
              CNsplinetype(ival));
      property->flag1 = property->flag1 | CNsplinetyp;
      property->splinetyp = ival;
      break;
   case DSapfill :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->applyfill = sval;
      property->flag1 = property->flag1 | CNapplyfill;
      break;

   case DSprintID :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag2 = property->flag2 | CNpr_ptID;
      property->pr_ptID = sval;
      property->flag2 = property->flag2 | CNpr_ndID;
      property->pr_ndID = sval;
      property->flag2 = property->flag2 | CNpr_trID;
      property->pr_trID = sval;
      property->flag2 = property->flag2 | CNpr_rtID;
      property->pr_rtID = sval;
      property->flag2 = property->flag2 | CNpr_rgID;
      property->pr_rgID = sval;
      property->flag2 = property->flag2 | CNpr_cvID;
      property->pr_cvID = sval;
      break;
   case DSpr_ptID :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag2 = property->flag2 | CNpr_ptID;
      property->pr_ptID = sval;
      break;
   case DSpr_ndID :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag2 = property->flag2 | CNpr_ndID;
      property->pr_ndID = sval;
      break;
   case DSpr_trID :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag2 = property->flag2 | CNpr_trID;
      property->pr_trID = sval;
      break;
   case DSpr_rtID :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag2 = property->flag2 | CNpr_rtID;
      property->pr_rtID = sval;
      break;
   case DSpr_rgID :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag2 = property->flag2 | CNpr_rgID;
      property->pr_rgID = sval;
      break;
   case DSpr_cvID :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag2 = property->flag2 | CNpr_cvID;
      property->pr_cvID = sval;
      break;

   case DShbnwidth :
      CNassign_double_keyword(&dval ,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      property->flag2  = property->flag2 | CNbinwidth;
      property->binwidth  = dval;
      break;
   case DShbnstart :
      CNassign_double_keyword(&dval ,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      property->flag2  = property->flag2 | CNbinstart;
      property->binstart  = dval;
      break;
   case DSbarmin :
      CNassign_double_keyword(&dval ,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      property->flag2  = property->flag2 | CNbarmin;
      property->barmin = dval;
      break;
   case DSplotann :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->flag2 = property->flag2 | CNplotannot;
      property->plotannot = sval;
      break;
   default :
      match = 0;
      break;
   }

   /* Return status */
   return(match);
}

/*
 * Print the list of allowable keywords
 */
void CNprint_dataset_keywords(verbose)
int verbose;
{
   CNprint_keywords(dataset_keywords,DATASET_MAXKEY,"Dataset",verbose);
}


/*
 * Print the list of keywords that have been set
 */
void CNwrite_dataset_options(fp,prop)
FILE *fp;
CNdataset_property *prop;
{
   CNdataset_property def_prop;
 
   /* Set default properties */
   CNset_default_dataset_property(&def_prop);
 
   /* Contour specific properties */
   if (prop->stepmethod == CN_STEPSIZE) {
      if ((prop->flag1 & CNcstep) != 0)
      (void) fprintf(fp,"%% cstep     = %g\n",prop->cstep    );
   } else if (prop->stepmethod == CN_NUMSTEPS) {
      if ((prop->flag1 & CNnstep) != 0)
      (void) fprintf(fp,"%% nsteps    = %d\n",prop->nsteps   );
   } else if (prop->stepmethod == CN_LOGSTEPS) {
      if ((prop->flag1 & CNlogzstep) != 0)
      (void) fprintf(fp,"%% logzstep  = %d\n",prop->logzstep );
   /*EMPTY*/
   } else if (prop->stepmethod == CN_USERDEFN) {
      /* This is handled elsewhere */
   }

   /* Print out each field if it differs from the default value */
   if (prop->contstyle != def_prop.contstyle)
   (void) fprintf(fp,"%% contstyle = %d\n",prop->contstyle);
  
   if (prop->logx != def_prop.logx)
   (void) fprintf(fp,"%% logx      = %s\n",BOOLEAN_VALUE(prop->logx));

   if (prop->logy != def_prop.logy)
   (void) fprintf(fp,"%% logy      = %s\n",BOOLEAN_VALUE(prop->logy));

   if (prop->logz != def_prop.logz)
   (void) fprintf(fp,"%% logz      = %s\n",BOOLEAN_VALUE(prop->logz));

   if (prop->linetypes != def_prop.linetypes)
   (void) fprintf(fp,"%% linetypes = %d\n",prop->linetypes);

   if (prop->contlabel != def_prop.contlabel)
   (void) fprintf(fp,"%% contlabel = %s\n",BOOLEAN_VALUE(prop->contlabel));

   if (prop->contintrp != def_prop.contintrp)
   (void) fprintf(fp,"%% interp    = %d\n",prop->contintrp);

   if (prop->contclip  != def_prop.contclip )
   (void) fprintf(fp,"%% contclip  = %s\n",BOOLEAN_VALUE(prop->contclip ));

   if (prop->contbitmap!= def_prop.contbitmap)
   (void) fprintf(fp,"%% bitmap    = %s\n",BOOLEAN_VALUE(prop->contbitmap));

   if (prop->meshplot  != def_prop.meshplot )
   (void) fprintf(fp,"%% meshplot  = %s\n",BOOLEAN_VALUE(prop->meshplot ));

   if (prop->boundary  != def_prop.boundary )
   (void) fprintf(fp,"%% boundary  = %s\n",BOOLEAN_VALUE(prop->boundary ));

   if (prop->regbound  != def_prop.regbound )
   (void) fprintf(fp,"%% regbound  = %s\n",BOOLEAN_VALUE(prop->regbound ));

   if (prop->fillbnd   != def_prop.fillbnd  )
   (void) fprintf(fp,"%% fillbnd   = %s\n",BOOLEAN_VALUE(prop->fillbnd  ));

   if (prop->vlog      != def_prop.vlog     )
   (void) fprintf(fp,"%% vlog      = %s\n",BOOLEAN_VALUE(prop->vlog     ));

   if (prop->vhead     != def_prop.vhead    )
   (void) fprintf(fp,"%% vhead     = %s\n",BOOLEAN_VALUE(prop->vhead    ));

   if (prop->vtail     != def_prop.vtail    )
   (void) fprintf(fp,"%% vtail     = %s\n",BOOLEAN_VALUE(prop->vtail    ));

   if (prop->splinetyp != def_prop.splinetyp)
   (void) fprintf(fp,"%% spline    = %d\n",prop->splinetyp);

   if (prop->applyfill != def_prop.applyfill)
   (void) fprintf(fp,"%% applyfill = %s\n",BOOLEAN_VALUE(prop->applyfill));

   if (prop->pr_ptID   != def_prop.pr_ptID  )
   (void) fprintf(fp,"%% pointID   = %s\n",BOOLEAN_VALUE(prop->pr_ptID  ));

   if (prop->pr_ndID   != def_prop.pr_ndID  )
   (void) fprintf(fp,"%% nodeID    = %s\n",BOOLEAN_VALUE(prop->pr_ndID  ));

   if (prop->pr_trID   != def_prop.pr_trID  )
   (void) fprintf(fp,"%% triaID    = %s\n",BOOLEAN_VALUE(prop->pr_trID  ));

   if (prop->pr_rtID   != def_prop.pr_rtID  )
   (void) fprintf(fp,"%% rectID    = %s\n",BOOLEAN_VALUE(prop->pr_rtID  ));

   if (prop->pr_rgID   != def_prop.pr_rgID  )
   (void) fprintf(fp,"%% regionID  = %s\n",BOOLEAN_VALUE(prop->pr_rgID  ));

   if (prop->pr_cvID   != def_prop.pr_cvID  )
   (void) fprintf(fp,"%% curveID   = %s\n",BOOLEAN_VALUE(prop->pr_cvID  ));

   if (prop->binwidth  != def_prop.binwidth )
   (void) fprintf(fp,"%% binwidth  = %g\n",prop->binwidth);

   if (prop->binstart  != def_prop.binstart )
   (void) fprintf(fp,"%% binstart  = %g\n",prop->binstart);

   if (prop->barmin  != def_prop.barmin )
   (void) fprintf(fp,"%% barmin    = %g\n",prop->barmin);

   if (prop->plotannot != def_prop.plotannot)
   (void) fprintf(fp,"%% annotate  = %s\n",BOOLEAN_VALUE(prop->plotannot));

   /* Derived quantities - print only if the flag is set */
   if ((prop->flag1 & CNcmax) != 0)
   (void) fprintf(fp,"%% cmax      = %f\n",prop->cmax     );

   if ((prop->flag1 & CNcmin) != 0)
   (void) fprintf(fp,"%% cmin      = %f\n",prop->cmin     );

   if ((prop->flag1 & CNvlogscale) != 0)
   (void) fprintf(fp,"%% vlogscale = %g\n",prop->vlogscale);

   if ((prop->flag1 & CNvscale) != 0)
   (void) fprintf(fp,"%% vscale = %g\n",prop->vscale);
}



/*
 * Properties or data-options associated to a global-curve
 */

/*
 * Delete a gb-curve property structure
 */
void CNdelete_gbcurve_property(prop)
CNgbcurve_property *prop;
{
   if (prop!=NULL) {
      if (prop->lnlabel) free((char *)prop->lnlabel);
      free((char *)prop);
   }
}


/*
 * Delete the fields in a gb-curve property structure
 */
void CNdelete_gbcurve_property_fields(prop)
CNgbcurve_property *prop;
{
   if (prop!=NULL) {
      if (prop->lnlabel) free((char *)prop->lnlabel);
   }
}

/*
 * Set the default property of a gbcurve
 */
void CNset_default_gbcurve_property(prop)
CNgbcurve_property *prop;
{
   /* Flag */
   prop->flag      = 0;

   /* Line label */
   prop->lnlabel   = NULL;

   /* Line changes */
   prop->minlntyp  = CN_LN_SOLID;
   prop->maxlntyp  = CN_LN_TYPES;
   prop->linechg   = CN_FALSE;

   /* Lines */
   prop->lnwidth   = 1;
   prop->lntype    = CN_LN_SOLID;
   prop->lncolor   = 1;

   /* Marker changes */
   prop->minmktyp  = 1;
   prop->maxmktyp  = CN_MK_TYPES;
   prop->markchg   = CN_FALSE;

   /* Markers */
   prop->mksize    = 1;
   prop->mktype    = CN_MK_NONE;
   prop->mkcolor   = 1;

   /* Fill changes */
   prop->minfltyp  = 1;
   prop->maxfltyp  = CN_FILL_TYPES;
   prop->fillchg   = CN_FALSE;

   /* Fills */
   prop->fltype    = CN_FILL_NONE;
   prop->flcolor   = 1;
}

/*
 * Print the values of a global-curve property, primarily for debugging
 */
void CNprint_gbcurve_property(prop)
CNgbcurve_property *prop;
{
   (void) fprintf(stdout,"dlinelabel   = \"%s\"\n",prop->lnlabel);
   (void) fprintf(stdout,"dlinemin     = %d\n",prop->minlntyp);
   (void) fprintf(stdout,"dlinemax     = %d\n",prop->maxlntyp);
   (void) fprintf(stdout,"dlinechange  = %s\n",BOOLEAN_VALUE(prop->linechg));
   (void) fprintf(stdout,"dlinewidth   = %d\n",prop->lnwidth);
   (void) fprintf(stdout,"dlinetype    = %d\n",prop->lntype);
   (void) fprintf(stdout,"dlinecolor   = %d\n",prop->lncolor);
   (void) fprintf(stdout,"dmarkermin   = %d\n",prop->minmktyp);
   (void) fprintf(stdout,"dmarkermax   = %d\n",prop->maxmktyp);
   (void) fprintf(stdout,"dmarkerchange= %s\n",BOOLEAN_VALUE(prop->markchg));
   (void) fprintf(stdout,"dmarkersize  = %d\n",prop->mksize);
   (void) fprintf(stdout,"dmarkertype  = %d\n",prop->mktype);
   (void) fprintf(stdout,"dmarkercolor = %d\n",prop->mkcolor);
   (void) fprintf(stdout,"dfillmin     = %d\n",prop->minfltyp);
   (void) fprintf(stdout,"dfillmax     = %d\n",prop->maxfltyp);
   (void) fprintf(stdout,"dfillchange  = %s\n",BOOLEAN_VALUE(prop->fillchg));
   (void) fprintf(stdout,"dfilltype    = %d\n",prop->fltype);
   (void) fprintf(stdout,"dfillcolor   = %d\n",prop->flcolor);
}


/*
 * Set the property of a global-curve
 *    This is done by copying the contents of one global-curve to another
 *    based on one global-curve's flag.
 */
void CNset_gbcurve_property(C1,C2)
CNgbcurve_property *C1, *C2;
{
  /* Error checking */
  if (C1==NULL || C2==NULL) {
     (void) fprintf(stderr,
     "Error in CNset_gbcurve_property() : Null gbcurve property!\n");
     return;
  }

  C1->flag = C1->flag | C2->flag;
  if ((C2->flag & CNlnlabel) != 0) {
      CNdestroy_string(C1->lnlabel);
      C1->lnlabel  = CNcreate_string(C2->lnlabel);
  }
  if ((C2->flag & CNminlntyp  ) != 0) C1->minlntyp  = C2->minlntyp;
  if ((C2->flag & CNmaxlntyp  ) != 0) C1->maxlntyp  = C2->maxlntyp;
  if ((C2->flag & CNlinechg   ) != 0) C1->linechg   = C2->linechg;
  if ((C2->flag & CNlnwidth   ) != 0) C1->lnwidth   = C2->lnwidth;
  if ((C2->flag & CNlntype    ) != 0) C1->lntype    = C2->lntype ;
  if ((C2->flag & CNlncolor   ) != 0) C1->lncolor   = C2->lncolor;
  if ((C2->flag & CNminmktyp  ) != 0) C1->minmktyp  = C2->minmktyp;
  if ((C2->flag & CNmaxmktyp  ) != 0) C1->maxmktyp  = C2->maxmktyp;
  if ((C2->flag & CNmarkchg   ) != 0) C1->markchg   = C2->markchg;
  if ((C2->flag & CNmksize    ) != 0) C1->mksize    = C2->mksize ;
  if ((C2->flag & CNmktype    ) != 0) C1->mktype    = C2->mktype ;
  if ((C2->flag & CNmkcolor   ) != 0) C1->mkcolor   = C2->mkcolor;
  if ((C2->flag & CNminfltyp  ) != 0) C1->minfltyp  = C2->minfltyp;
  if ((C2->flag & CNmaxfltyp  ) != 0) C1->maxfltyp  = C2->maxfltyp;
  if ((C2->flag & CNfillchg   ) != 0) C1->fillchg   = C2->fillchg;
  if ((C2->flag & CNfltype    ) != 0) C1->fltype    = C2->fltype ;
  if ((C2->flag & CNflcolor   ) != 0) C1->flcolor   = C2->flcolor;
}

static enum gbcurve_keyvals {
   GBlnlabel,
   GBlnmin,
   GBlnmax,
   GBlnchg,
   GBlnwidth,
   GBlntype,
   GBlncolor,
   GBmkmin,
   GBmkmax,
   GBmkchg,
   GBmksize,
   GBmktype,
   GBmkcolor,
   GBflmin,
   GBflmax,
   GBflchg,
   GBfltype,
   GBflcolor
};
#define GBCURVE_MAXKEY 24
static CNkeyword gbcurve_keywords[] = {
   {"dlinelabel",   GBlnlabel, CN_STRING , 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"dlinemin",     GBlnmin,   CN_INTEGER, 1, 1, 1, 0,CN_LN_TYPES, 0.0,0.0,0.0},
   {"dlinemax",     GBlnmax,   CN_INTEGER, 1, 1, 1, 0,CN_LN_TYPES, 0.0,0.0,0.0},
   {"dlinechange",  GBlnchg,   CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"dlinewidth",   GBlnwidth, CN_INTEGER, 1, 1, 1, 0,50, 0.0, 0.0, 0.0},
   {"dlinetype",    GBlntype,  CN_INTEGER, 1, 0, 1, 0, 0, 0.0, 0.0, 0.0},
   {"dlinecolor",   GBlncolor, CN_INTEGER, 1, 0, 1,-1, 0, 0.0, 0.0, 0.0},
   {"dmarkermin",   GBmkmin,   CN_INTEGER, 1, 1, 1, 0,CN_MK_TYPES, 0.0,0.0,0.0},
   {"dmarkmin",     GBmkmin,   CN_INTEGER, 1, 1, 1, 0,CN_MK_TYPES, 0.0,0.0,0.0},
   {"dmarkermax",   GBmkmax,   CN_INTEGER, 1, 1, 1, 0,CN_MK_TYPES, 0.0,0.0,0.0},
   {"dmarkmax",     GBmkmax,   CN_INTEGER, 1, 1, 1, 0,CN_MK_TYPES, 0.0,0.0,0.0},
   {"dmarkerchange",GBmkchg,   CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"dmarkchange",  GBmkchg,   CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"dmarkersize",  GBmksize,  CN_INTEGER, 1, 1, 1, 1,10, 0.0, 0.0, 0.0},
   {"dmarksize",    GBmksize,  CN_INTEGER, 1, 1, 1, 1,10, 0.0, 0.0, 0.0},
   {"dmarkertype"  ,GBmktype,  CN_INTEGER, 1, 0, 1, 0, 0, 0.0, 0.0, 0.0},
   {"dmarktype"  ,  GBmktype,  CN_INTEGER, 1, 0, 1, 0, 0, 0.0, 0.0, 0.0},
   {"dmarkercolor", GBmkcolor, CN_INTEGER, 1, 0, 1,-1, 0, 0.0, 0.0, 0.0},
   {"dmarkcolor",   GBmkcolor, CN_INTEGER, 1, 0, 1,-1, 0, 0.0, 0.0, 0.0},
   {"dfillmin",     GBflmin,   CN_INTEGER, 1, 1, 1, 0,CN_FL_TYPES, 0.0,0.0,0.0},
   {"dfillmax",     GBflmax,   CN_INTEGER, 1, 1, 1, 0,CN_FL_TYPES, 0.0,0.0,0.0},
   {"dfillchange",  GBflchg,   CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"dfilltype",    GBfltype,  CN_INTEGER, 1, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"dfillcolor",   GBflcolor, CN_INTEGER, 1, 0, 1,-1, 0, 0.0, 0.0, 0.0},
};

/*
 * Act on a curve-keyword (argument=value) and change the global-curve property
 */
int CNparse_gbcurve_property(property,argument,value,verbose)
CNgbcurve_property  *property;           /* Global-Curve property */
char                *argument,*value;
int                 verbose;
{
   CNkeyword *key;
   int       match = 0, keyval = -1, i;
   char      cval[CN_MAXCHAR];
   short     sval=0;
   int       ival=0;

   /* Search for a matching argument keyword */
   for (i=0; i<GBCURVE_MAXKEY && !match; i++) {
      if (strcmp(gbcurve_keywords[i].keyword,argument) == 0) {
         match = 1;
         keyval = gbcurve_keywords[i].keyval;
         key    = &(gbcurve_keywords[i]);
      }
   }

   /* Return if not found */
   if (match == 0) return(match);

   /* Now get the corresponding value and change the structure */
   switch (keyval) {
   case GBlnlabel :
      CNassign_string_keyword(cval,value,argument,verbose);
      property->flag = property->flag | CNlnlabel;
      CNdestroy_string(property->lnlabel);
      property->lnlabel  = CNcreate_string(cval);
      break;
   case GBlnmin :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      property->flag = property->flag | CNminlntyp;
      property->minlntyp = ival;
      break;
   case GBlnmax :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      property->flag = property->flag | CNmaxlntyp;
      property->maxlntyp = ival;
      break;
   case GBlnchg :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->linechg = sval;
      if (property->linechg)
      property->flag = property->flag | CNlinechg;
      else
      property->flag = property->flag | CNlntype;
      break;
   case GBlnwidth :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      if (ival > CN_LN_SIZES)
         ival = CN_LN_SIZES;
      property->flag = property->flag | CNlnwidth;
      property->lnwidth = ival;
      break;
   case GBlntype :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      if (ival > CN_LN_TYPES)
         ival = (ival - 1) % CN_LN_TYPES + 1;
      property->flag = property->flag | CNlntype;
      property->lntype = ival;
      property->linechg= 0;
      break;
   case GBlncolor :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      if (ival > CN_LN_COLRS)
         ival = (ival - 1) % CN_LN_COLRS + 1;
      property->flag = property->flag | CNlncolor;
      property->lncolor = ival;
      break;
   case GBmkmin :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      property->flag = property->flag | CNminmktyp;
      property->minmktyp = ival;
      break;
   case GBmkmax :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      property->flag = property->flag | CNmaxmktyp;
      property->maxmktyp = ival;
      break;
   case GBmkchg :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->markchg = sval;
      if (property->markchg)
      property->flag = property->flag | CNmarkchg;
      property->flag = property->flag | CNmktype;
      break;
   case GBmksize :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      if (ival > CN_MK_SIZES)
         ival = CN_MK_SIZES;
      property->flag = property->flag | CNmksize;
      property->mksize = ival;
      property->markchg= 0;
      break;
   case GBmktype :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      if (ival > CN_MK_TYPES)
         ival = (ival - 1) % CN_MK_TYPES + 1;
      property->flag = property->flag | CNmktype;
      property->mktype = ival;
      property->markchg= 0;
      break;
   case GBmkcolor :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      if (ival > CN_MK_COLRS)
         ival = (ival - 1) % CN_MK_COLRS + 1;
      property->flag = property->flag | CNmkcolor;
      property->mkcolor = ival;
      break;
   case GBflmin :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      property->flag = property->flag | CNminfltyp;
      property->minfltyp = ival;
      break;
   case GBflmax :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      property->flag = property->flag | CNmaxfltyp;
      property->maxfltyp = ival;
      break;
   case GBflchg :
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      property->fillchg = sval;
      if (property->fillchg)
      property->flag = property->flag | CNfillchg;
      else
      property->flag = property->flag | CNfltype;
      break;
   case GBfltype :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      if (ival > CN_FL_TYPES)
         ival = (ival - 1) % CN_FL_TYPES + 1;
      property->flag = property->flag | CNfltype;
      property->fltype = ival;
      property->markchg= 0;
      break;
   case GBflcolor :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      if (ival > CN_FL_COLRS)
         ival = (ival - 1) % CN_FL_COLRS + 1;
      property->flag = property->flag | CNflcolor;
      property->flcolor = ival;
      /*
      property->flag = property->flag | CNfillchg;
      property->fillchg= 0;
       */
      break;
   default :
      match = 0;
      break;
   }
 
   /* Return status */
   return(match);
}


/*
 * Print the list of allowable keywords
 */
void CNprint_gbcurve_keywords(verbose)
int verbose;
{
   CNprint_keywords(gbcurve_keywords,GBCURVE_MAXKEY,"GbCurve",verbose);
}
 


/*
 * Properties or data-options associated to a curve
 */

/*
 * Allocate room for a curve property structure
 */
CNcurve_property *CNmake_curve_property()
{
   CNcurve_property *newptr;
   unsigned int size = sizeof(CNcurve_property);
 
   if ((newptr = (CNcurve_property *)malloc(size))!=NULL) {
      CNset_default_curve_property(newptr);
   }
   return(newptr);
}


/*
 * Delete a curve property structure
 */
void CNdelete_curve_property(prop)
CNcurve_property *prop;
{
   if (prop!=NULL) {
      if (prop->linelabel) free((char *)prop->linelabel);
      free((char *)prop);
   }
}


/*
 * Delete the fields in a curve property structure
 */
void CNdelete_curve_property_fields(prop)
CNcurve_property *prop;
{
   if (prop!=NULL) {
      if (prop->linelabel) free((char *)prop->linelabel);
   }
}

/*
 * Set the default property of a curve
 */
void CNset_default_curve_property(prop)
CNcurve_property *prop;
{
   /* Flag */
   prop->flag      = 0;

   /* Lines */
   prop->linelabel  = NULL;
   prop->linewidth  = 1;
   prop->linetype   = CN_LN_SOLID;
   prop->linecolor  = 1;

   /* Markers */
   prop->marksize   = 1;
   prop->marktype   = CN_MK_NONE;
   prop->markcolor  = 1;

   /* Fill */
   prop->filltype   = CN_FILL_NONE;
   prop->fillcolor  = 1;
}


/*
 * Print the values of a curve property, primarily for debugging purposes
 */
void CNprint_curve_property(prop)
CNcurve_property *prop;
{
   (void) fprintf(stdout,"Label      = \"%s\"\n",prop->linelabel);
   (void) fprintf(stdout,"Linewidth  = %d\n",prop->linewidth);
   (void) fprintf(stdout,"Linetype   = %d\n",prop->linetype);
   (void) fprintf(stdout,"Linecolor  = %d\n",prop->linecolor);
   (void) fprintf(stdout,"Markersize = %d\n",prop->marksize);
   (void) fprintf(stdout,"Markertype = %d\n",prop->marktype);
   (void) fprintf(stdout,"Markercolor= %d\n",prop->markcolor);
   (void) fprintf(stdout,"Filltype   = %d\n",prop->filltype);
   (void) fprintf(stdout,"Fillcolor  = %d\n",prop->fillcolor);
}


/*
 * Set the property of a curve
 *    This is done by copying the contents of one curve to another
 *    based on one curve's flag.
 */
void CNset_curve_property(C1,C2)
CNcurve_property *C1, *C2;
{
  /* Error checking */
  if (C1==NULL || C2==NULL) {
     (void) fprintf(stderr,
     "Error in CNset_curve_property() : Null curve property!\n");
     return;
  }

  C1->flag = C1->flag | C2->flag;
  if ((C2->flag & CNlinelabel) != 0) {
      CNdestroy_string(C1->linelabel);
      C1->linelabel  = CNcreate_string(C2->linelabel);
  }
  if ((C2->flag & CNlinewidth) != 0) C1->linewidth= C2->linewidth;
  if ((C2->flag & CNlinetype ) != 0) C1->linetype = C2->linetype;
  if ((C2->flag & CNlinecolor) != 0) C1->linecolor= C2->linecolor;
  if ((C2->flag & CNmarksize ) != 0) C1->marksize = C2->marksize;
  if ((C2->flag & CNmarktype ) != 0) C1->marktype = C2->marktype;
  if ((C2->flag & CNmarkcolor) != 0) C1->markcolor= C2->markcolor;
  if ((C2->flag & CNfilltype ) != 0) C1->filltype = C2->filltype ;
  if ((C2->flag & CNfillcolor) != 0) C1->fillcolor= C2->fillcolor;
}


/*
 * Copy the property of a curve
 *    This is done by copying the contents of one curve to another
 *    The curve flag is ignored.
 */
void CNcopy_curve_property(C1,C2)
CNcurve_property *C1, *C2;
{
  /* Error checking */
  if (C1==NULL || C2==NULL) {
     (void) fprintf(stderr,
     "Error in CNcopy_curve_property() : Null curve property!\n");
     return;
  }

  C1->flag = C2->flag;
  CNdestroy_string(C1->linelabel);
  if (C2->linelabel) C1->linelabel  = CNcreate_string(C2->linelabel);
  C1->linewidth= C2->linewidth;
  C1->linetype = C2->linetype;
  C1->linecolor= C2->linecolor;
  C1->marksize = C2->marksize;
  C1->marktype = C2->marktype;
  C1->markcolor= C2->markcolor;
  C1->filltype = C2->filltype ;
  C1->fillcolor= C2->fillcolor;
}


static enum curve_keyvals {
   CVlnlabel,
   CVlnwidth,
   CVlntype,
   CVlncolor,
   CVmksize,
   CVmktype,
   CVmkcolor,
   CVfltype,
   CVflcolor
};
#define CURVE_MAXKEY 21
static CNkeyword curve_keywords[] = {
   {"linelabel",  CVlnlabel, CN_STRING , 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"linewidth",  CVlnwidth, CN_INTEGER, 1, 1, 1, 0,32, 0.0, 0.0, 0.0},
   {"lw",         CVlnwidth, CN_INTEGER, 1, 1, 1, 0,32, 0.0, 0.0, 0.0},
   {"linetype",   CVlntype,  CN_INTEGER, 1, 0, 1, 0, 0, 0.0,0.0,0.0},
   {"lt",         CVlntype,  CN_INTEGER, 1, 0, 1, 0, 0, 0.0,0.0,0.0},
   {"linestyle",  CVlntype,  CN_INTEGER, 1, 0, 1, 0, 0, 0.0,0.0,0.0},
   {"linecolor",  CVlncolor, CN_INTEGER, 1, 0, 1,-1, 0, 0.0,0.0,0.0},
   {"lc",         CVlncolor, CN_INTEGER, 1, 0, 1,-1, 0, 0.0,0.0,0.0},
   {"markersize", CVmksize,  CN_INTEGER, 1, 1, 1, 1,10, 0.0,0.0,0.0},
   {"marksize",   CVmksize,  CN_INTEGER, 1, 1, 1, 1,10, 0.0,0.0,0.0},
   {"ms",         CVmksize,  CN_INTEGER, 1, 1, 1, 1,10, 0.0,0.0,0.0},
   {"markertype", CVmktype,  CN_INTEGER, 1, 0, 1, 0, 0, 0.0,0.0,0.0},
   {"marktype",   CVmktype,  CN_INTEGER, 1, 0, 1, 0, 0, 0.0,0.0,0.0},
   {"mt",         CVmktype,  CN_INTEGER, 1, 0, 1, 0, 0, 0.0,0.0,0.0},
   {"markerstyle",CVmktype,  CN_INTEGER, 1, 0, 1, 0, 0, 0.0,0.0,0.0},
   {"markercolor",CVmkcolor, CN_INTEGER, 1, 0, 1,-1, 0, 0.0,0.0,0.0},
   {"markcolor",  CVmkcolor, CN_INTEGER, 1, 0, 1,-1, 0, 0.0,0.0,0.0},
   {"mc",         CVmkcolor, CN_INTEGER, 1, 0, 1,-1, 0, 0.0,0.0,0.0},
   {"filltype",   CVfltype,  CN_INTEGER, 1, 0, 0, 0, 0, 0.0,0.0,0.0},
   {"fillstyle",  CVfltype,  CN_INTEGER, 1, 0, 0, 0, 0, 0.0,0.0,0.0},
   {"fillcolor",  CVflcolor, CN_INTEGER, 1, 0, 1,-1, 0, 0.0,0.0,0.0},
};

/*
 * Act on a curve-keyword (argument=value) and change the curve property
 */
int CNparse_curve_property(property,argument,value,verbose)
CNcurve_property  *property;           /* Curve property */
char              *argument,*value;
int               verbose;
{
   CNkeyword *key;
   int       match = 0, keyval = -1, i;
   char      cval[CN_MAXCHAR];
   int       ival=0;

   /* Search for a matching argument keyword */
   for (i=0; i<CURVE_MAXKEY && !match; i++) {
      if (strcmp(curve_keywords[i].keyword,argument) == 0) {
         match = 1;
         keyval = curve_keywords[i].keyval;
         key    = &(curve_keywords[i]);
      }
   }

   /* Return if not found */
   if (match == 0) return(match);

   /* Now get the corresponding value and change the structure */
   switch (keyval) {
   case CVlnlabel :
      CNassign_string_keyword(cval,value,argument,verbose);
      property->flag = property->flag | CNlinelabel;
      CNdestroy_string(property->linelabel);
      property->linelabel  = CNcreate_string(cval);
      break;
   case CVlnwidth :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      if (ival > CN_LN_SIZES)
         ival = CN_LN_SIZES;
      property->flag = property->flag | CNlinewidth;
      property->linewidth = ival;
      break;
   case CVlntype :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      if (ival > CN_LN_TYPES)
         ival = (ival - 1) % CN_LN_TYPES + 1;
      property->flag = property->flag | CNlinetype;
      property->linetype = ival;
      break;
   case CVlncolor :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      if (ival > CN_LN_COLRS)
         ival = (ival - 1) % CN_LN_COLRS + 1;
      property->flag = property->flag | CNlinecolor;
      property->linecolor = ival;
      break;
   case CVmksize :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      if (ival > CN_MK_SIZES)
         ival = CN_MK_SIZES;
      property->flag = property->flag | CNmarksize;
      property->marksize = ival;
      break;
   case CVmktype :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      if (ival > CN_MK_TYPES)
         ival = (ival - 1) % CN_MK_TYPES + 1;
      property->flag = property->flag | CNmarktype;
      property->marktype = ival;
      break;
   case CVmkcolor :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      if (ival > CN_MK_COLRS)
         ival = (ival - 1) % CN_MK_COLRS + 1;
      property->flag = property->flag | CNmarkcolor;
      property->markcolor = ival;
      break;
   case CVfltype :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      if (ival > CN_FL_TYPES)
         ival = (ival - 1) % CN_FL_TYPES + 1;
      property->flag = property->flag | CNfilltype;
      property->filltype = ival;
      break;
   case CVflcolor :
      CNassign_int_keyword(&ival,value,argument,
                           key->ival, key->imin, key->imax,
                           key->chkmin, key->chkmax, verbose);
      if (ival > CN_FL_COLRS)
         ival = (ival - 1) % CN_FL_COLRS + 1;
      property->flag = property->flag | CNfillcolor;
      property->fillcolor = ival;
      break;
   default :
      match = 0;
      break;
   }

   /* Return status */
   return(match);
}

/*
 * Print the list of allowable keywords
 */
void CNprint_curve_keywords(verbose)
int verbose;
{
   CNprint_keywords(curve_keywords,CURVE_MAXKEY,"Curve",verbose);
}

/*
 * Print the list of keywords that have been set
 */
void CNwrite_curve_options(fp,prop)
FILE *fp;
CNcurve_property *prop;
{
   CNcurve_property def_prop;
 
   /* Set default properties */
   CNset_default_curve_property(&def_prop);
 
   /* Print out each field if it differs from the default value */
   if (prop->linelabel)
   (void) fprintf(fp,"%% linelabel = \"%s\"\n",prop->linelabel   );

   if (prop->linewidth != def_prop.linewidth)
   (void) fprintf(fp,"%% linewidth = %d\n",prop->linewidth);

   if (prop->linetype  != def_prop.linetype )
   (void) fprintf(fp,"%% linetype  = %d\n",prop->linetype );

   if (prop->linecolor != def_prop.linecolor)
   (void) fprintf(fp,"%% linecolor = %d\n",prop->linecolor);

   if (prop->marksize  != def_prop.marksize )
   (void) fprintf(fp,"%% markersize  = %d\n",prop->marksize );

   if (prop->marktype  != def_prop.marktype )
   (void) fprintf(fp,"%% markertype  = %d\n",prop->marktype );

   if (prop->markcolor != def_prop.markcolor)
   (void) fprintf(fp,"%% markercolor = %d\n",prop->markcolor);

   if (prop->filltype  != def_prop.filltype )
   (void) fprintf(fp,"%% filltype  = %d\n",prop->filltype );

   if (prop->fillcolor != def_prop.fillcolor)
   (void) fprintf(fp,"%% fillcolor = %d\n",prop->fillcolor);
}


/*
 * VIEW PROPERTIES
 */

/*
 * Set the property of a view 
 *    This is done by copying the contents of one view to another
 *    based on one view's flag.
 */
void CNset_view_property(P1,P2)
CNviewptr P1, P2;
{
   /* Error checking */
   if (P1==NULL || P2==NULL) {
      (void) fprintf(stderr,
      "Error in CNset_view_property() : Null view structure!\n");
      return;
   }

   /* A zero flag means there is nothing new in the view */
   if (P2->flag == 0) return;

   P1->flag = P1->flag | P2->flag;
   if ((P2->flag & CNeyepos ) != 0)  P1->eyepos     = P2->eyepos;
   if ((P2->flag & CNviewctr) != 0)  P1->viewcenter = P2->viewcenter;
   if ((P2->flag & CNwindow ) != 0)  {
      P1->windscl_xl  = P2->windscl_xl;
      P1->windscl_xr  = P2->windscl_xr;
      P1->windscl_yb  = P2->windscl_yb;
      P1->windscl_yt  = P2->windscl_yt;
   }
   if ((P2->flag & CNleft_hw) != 0)  {
      P1->left_handed_world = P2->left_handed_world;
   }
   if ((P2->flag & CNaxisscl   ) != 0) {
      P1->axis_scale   = P2->axis_scale;
      P1->xaxis_scale  = P2->xaxis_scale;
      P1->yaxis_scale  = P2->yaxis_scale;
      P1->zaxis_scale  = P2->zaxis_scale;
   }
   if ((P2->flag & CNaxislbl   ) != 0) P1->axis_label   = P2->axis_label;
   if ((P2->flag & CNaxismvt   ) != 0) P1->axis_movement= P2->axis_movement;
   if ((P2->flag & CNaxisguides) != 0) P1->axis_guides  = P2->axis_guides  ;
   if ((P2->flag & CNhiddenln  ) != 0) P1->hiddenline   = P2->hiddenline   ;
   if ((P2->flag & CNpaintcb   ) != 0) P1->paint_cube   = P2->paint_cube   ;

   /* Initialize the view-transfo matrix */
   CNcalculate_view_transfo(P1);
}

static enum view_keyvals {
   VWeyepos_x,
   VWeyepos_y,
   VWeyepos_z,
   VWvwctr_x,
   VWvwctr_y,
   VWvwctr_z,
   VWwn_xmin,
   VWwn_xmax,
   VWwn_ymin,
   VWwn_ymax,
   VWleftwld,
   VWaxislbl,
   VWaxismv,
   VWaxisscl,
   VWxaxisscl,
   VWyaxisscl,
   VWzaxisscl,
   VWaxisgds,
   VWhdnline,
   VWptcube
};
#define VIEW_MAXKEY 31
static CNkeyword view_keywords[] = {
   {"eyepos.x",    VWeyepos_x,CN_DOUBLE,  0, 0, 0, 0, 0, 1.0, 0.0, 0.0},
   {"eyepos_x",    VWeyepos_x,CN_DOUBLE,  0, 0, 0, 0, 0, 1.0, 0.0, 0.0},
   {"eyepos.y",    VWeyepos_y,CN_DOUBLE,  0, 0, 0, 0, 0, 1.5, 0.0, 0.0},
   {"eyepos_y",    VWeyepos_y,CN_DOUBLE,  0, 0, 0, 0, 0, 1.5, 0.0, 0.0},
   {"eyepos.z",    VWeyepos_z,CN_DOUBLE,  0, 0, 0, 0, 0, 0.5, 0.0, 0.0},
   {"eyepos_z",    VWeyepos_z,CN_DOUBLE,  0, 0, 0, 0, 0, 0.5, 0.0, 0.0},
   {"viewcenter.x",VWvwctr_x, CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"viewcenter_x",VWvwctr_x, CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"viewcenter.y",VWvwctr_y, CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"viewcenter_y",VWvwctr_y, CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"viewcenter.z",VWvwctr_z, CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"viewcenter_z",VWvwctr_z, CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"window.xmin", VWwn_xmin, CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"window_xmin", VWwn_xmin, CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"window.xmax", VWwn_xmax, CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"window_xmax", VWwn_xmax, CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"window.ymin", VWwn_ymin, CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"window_ymin", VWwn_ymin, CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"window.ymax", VWwn_ymax, CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"window_ymax", VWwn_ymax, CN_DOUBLE,  0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"leftworld",   VWleftwld, CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"axislabel",   VWaxislbl, CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"axismove",    VWaxismv , CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"axisscale",   VWaxisscl, CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"xaxisscale",  VWxaxisscl,CN_DOUBLE,  1, 0, 0, 0, 0,1.00,1e-99,0.0},
   {"yaxisscale",  VWyaxisscl,CN_DOUBLE,  1, 0, 0, 0, 0,1.00,1e-99,0.0},
   {"zaxisscale",  VWzaxisscl,CN_DOUBLE,  1, 0, 0, 0, 0,1.00,1e-99,0.0},
   {"axisguide",   VWaxisgds, CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"axisguides",  VWaxisgds, CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"hiddenline",  VWhdnline, CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
   {"paintcube",   VWptcube,  CN_BOOLEAN, 0, 0, 0, 0, 0, 0.0, 0.0, 0.0},
};

/*
 * Act on a view-keyword (argument=value) and change the view
 */
int CNparse_view_property(view_params,argument,value,verbose)
CNviewptr view_params; 
char      *argument,*value;
int       verbose;
{
   CNkeyword *key;
   int       match = 0, keyval = -1, i;
   short     sval=0.0;
   double    dval=0.0, wind=0.0;

   /* Search for a matching argument keyword */
   for (i=0; i<VIEW_MAXKEY && !match; i++) {
      if (strcmp(view_keywords[i].keyword,argument) == 0) {
         match = 1;
         keyval = view_keywords[i].keyval;
         key    = &(view_keywords[i]);
      }
   }

   /* Return if not found */
   if (match == 0) return(match);

   /* window default */
   wind = 1.5*(view_params->xmax - view_params->xmin);

   /* Now get the corresponding value and change the view */
   switch (keyval) {
   case VWeyepos_x :
      CNassign_double_keyword(&dval ,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      view_params->eyepos.x = dval;
      view_params->flag = view_params->flag | CNeyepos;
      break;
   case VWeyepos_y :
      CNassign_double_keyword(&dval ,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      view_params->eyepos.y = dval;
      view_params->flag = view_params->flag | CNeyepos;
      break;
   case VWeyepos_z :
      CNassign_double_keyword(&dval ,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      view_params->eyepos.z = dval;
      view_params->flag = view_params->flag | CNeyepos;
      break;
   case VWvwctr_x :
      CNassign_double_keyword(&dval ,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      view_params->viewcenter.x = dval;
      view_params->flag = view_params->flag | CNviewctr;
      break;
   case VWvwctr_y :
      CNassign_double_keyword(&dval ,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      view_params->viewcenter.y = dval;
      view_params->flag = view_params->flag | CNviewctr;
      break;
   case VWvwctr_z :
      CNassign_double_keyword(&dval ,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      view_params->viewcenter.z = dval;
      view_params->flag = view_params->flag | CNviewctr;
      break;
   case VWwn_xmin :
      CNassign_double_keyword(&dval ,value,argument,
                           -wind, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      view_params->windscl_xl = dval;
      view_params->flag = view_params->flag | CNwindow;
      break;
   case VWwn_xmax :
      CNassign_double_keyword(&dval ,value,argument,
                           wind     , key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      view_params->windscl_xr = dval;
      view_params->flag = view_params->flag | CNwindow;
      break;
   case VWwn_ymin :
      CNassign_double_keyword(&dval ,value,argument,
                           -wind    , key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      view_params->windscl_yb = dval;
      view_params->flag = view_params->flag | CNwindow;
      break;
   case VWwn_ymax :
      CNassign_double_keyword(&dval ,value,argument,
                           wind, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      view_params->windscl_yt = dval;
      view_params->flag = view_params->flag | CNwindow;
      break;
   case VWleftwld : 
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      view_params ->left_handed_world = sval;
      view_params->flag = view_params->flag | CNleft_hw;
      break;
   case VWaxislbl : 
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      view_params ->axis_label = sval;
      view_params->flag = view_params->flag | CNaxislbl;
      break;
   case VWaxismv : 
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      view_params ->axis_movement = sval;
      view_params->flag = view_params->flag | CNaxismvt;
      break;
   case VWaxisscl : 
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      view_params ->axis_scale = sval;
      view_params->flag = view_params->flag | CNaxisscl;
      break;
   case VWxaxisscl :
      CNassign_double_keyword(&dval ,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      view_params->xaxis_scale = dval;
      view_params->axis_scale = CN_TRUE;
      view_params->flag = view_params->flag | CNaxisscl;
      break;
   case VWyaxisscl :
      CNassign_double_keyword(&dval ,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      view_params->yaxis_scale = dval;
      view_params->axis_scale = CN_TRUE;
      view_params->flag = view_params->flag | CNaxisscl;
      break;
   case VWzaxisscl :
      CNassign_double_keyword(&dval ,value,argument,
                           key->dval, key->dmin, key->dmax,
                           key->chkmin, key->chkmax, verbose);
      view_params->zaxis_scale = dval;
      view_params->axis_scale = CN_TRUE;
      view_params->flag = view_params->flag | CNaxisscl;
      break;
   case VWaxisgds : 
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      view_params ->axis_guides = sval;
      view_params->flag = view_params->flag | CNaxisguides;
      break;
   case VWhdnline : 
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      view_params ->hiddenline = sval;
      view_params->flag = view_params->flag | CNhiddenln;
      break;
   case VWptcube : 
      CNassign_boolean_keyword(&sval ,value,argument,verbose);
      view_params ->paint_cube = sval;
      view_params->flag = view_params->flag | CNpaintcb;
      break;
   default :
      match = 0;
      break;
   }

   /* return OK */
   return(match);
}

/*
 * Print the list of allowable keywords
 */
void CNprint_view_keywords(verbose)
int verbose;
{
   CNprint_keywords(view_keywords,VIEW_MAXKEY,"View",verbose);
}


/*
 * Print the list of keywords that have been set
 */
void CNwrite_view_options(fp,prop)
FILE *fp;
CNviewptr prop;
{
   CNviewptr def_prop;

   /* Create and initialize the view */
   if ((def_prop = CNcreate_view())==NULL) return;

   /* Print out each field if it differs from the default value */
   if (prop->left_handed_world != def_prop->left_handed_world)
   (void) fprintf(fp,"%% leftworld = %s\n",
                  BOOLEAN_VALUE(prop->left_handed_world));

   if (prop->axis_scale != def_prop->axis_scale)
   (void) fprintf(fp,"%% axisscale = %s\n",BOOLEAN_VALUE(prop->axis_scale));

   if (prop->xaxis_scale != def_prop->xaxis_scale)
   (void) fprintf(fp,"%% xaxisscale = %g\n",prop->xaxis_scale);

   if (prop->yaxis_scale != def_prop->yaxis_scale)
   (void) fprintf(fp,"%% yaxisscale = %g\n",prop->yaxis_scale);

   if (prop->zaxis_scale != def_prop->zaxis_scale)
   (void) fprintf(fp,"%% zaxisscale = %g\n",prop->zaxis_scale);

   if (prop->axis_label != def_prop->axis_label)
   (void) fprintf(fp,"%% axislabel = %s\n",BOOLEAN_VALUE(prop->axis_label));

   if (prop->axis_movement != def_prop->axis_movement)
   (void) fprintf(fp,"%% axismovement = %s\n",
                  BOOLEAN_VALUE(prop->axis_movement));

   if (prop->axis_guides != def_prop->axis_guides)
   (void) fprintf(fp,"%% axisguides= %s\n",BOOLEAN_VALUE(prop->axis_guides));

   if (prop->hiddenline != def_prop->hiddenline)
   (void) fprintf(fp,"%% hiddenline= %s\n",BOOLEAN_VALUE(prop->hiddenline));

   if (prop->paint_cube != def_prop->paint_cube)
   (void) fprintf(fp,"%% paintcube = %s\n",BOOLEAN_VALUE(prop->paint_cube));

   /* Delete the view */
   CNdelete_view(def_prop);
}
