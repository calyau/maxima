
/***************************************************
 *  
 *     I R I S P L O T
 *                       -------------const.h
 *
 *     Copright 1989  Zou Maorong 
 *
 ***************************************************/

#define BADVALUE                   (-1234567.0)

#define SAMPLE                     32
#define CURVESAMPLE                1000
#define TUBESAMPLE1                100
#define TUBESAMPLE2                12
#define MAPSAMPLE                  2000
#define EQNSAMPLE                  2000
#define EQNSTEP                    0.001
#define EQNSMALL                   0.0000000001
#define RK                         11
#define RKQC                       12

#define BEGIN_AN_OBJECT            20001
#define BEGIN_MATERIAL_DEFINITIONS 20002
#define BEGIN_GRAPHS               20003
#define DEFAULT_MATERIAL           20004
#define SET_VIEW                   20005


#define TUBE                       30000
#define SURFACE                    30001
#define CURVE                      30002
#define TMESH                      30003
#define CONTOUR                    30004
#define MAP                        30005
#define EQN                        30006

#define BMATERIAL                  40001
#define ROTATEX                    40002
#define ROTATEY                    40003
#define ROTATEZ                    40004
#define SCALE                      40005
#define TRANSLATE                  40006
#define GRAPH                      40007
#define TRANSPARANT                40008
#define OBLIQUE                    40009
#define PUSHMATRIX                 40010
#define POPMATRIX                  40011

#define PRETRANSLATION             40012
#define PREROTATION                40013
#define PRESCALING                 40014

#define COBJECT                    50001

#define BGNDEFINITION              80001
#define ENDDEFINITION              80002
#define BGNMATERIAL                80003
#define ENDMATERIAL                80004
#define NEWOBJECT                  80005
#define NEWMATERIAL                80006
#define WINDOW                     80007
#define OBJMENU                    80008
#define CENTER                     80009
#define FINALSTRUCTURE             80010

#define BGNLMODEL                  90007
#define ENDLMODEL                  90008
#define BGNLIGHT                   90009
#define ENDLIGHT                   90010
#define NEWLIGHT                   90011
#define ON                         90012

#define UNKNOWN_DATA               81001
#define CVN_DATA                   81001
#define POLYGON_DATA               81002
#define GRID_DATA                  81003
#define FEA_ANSYS                  81004
#define FEA_POLYGON                81005
#define FEA_FEAP                   81006

#ifndef EMISSION
#define EMISSION                   1
#define AMBIENT                    2
#define DIFUSE                     3
#define SPECULAR                   4
#define SHINNESS                   5
#define ALPHA                      7

#define LCOLOR	                   101
#define POSITION                   102
#define SPOTDIRECTION              103
#define SPOTLIGHT                  104
#define LOCALVIEWER                201
#define ATTENUATION                202
#define ATTENUATION2               203
#define TWOSIDE                    204
#endif

/*********************************************************************/
