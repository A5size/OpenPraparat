#define GLFW_INCLUDE_GLU

#include <iostream>
#include <math.h>
#include <GLFW/glfw3.h>
#include <pthread.h>

#include <vector>
#include <fstream>
#include <map>
#include <numeric>
#include <cmath>

//For Mac
#include <sstream>

//#include <stb_image.h>

//using namespace std;

//< --------------------------------------- >
//< ----------------imgui------------------ >
#include "imgui.h"
#include "imgui_impl_glfw.h"
#include "imgui_impl_opengl2.h"
#include <stdio.h>
#ifdef __APPLE__
#define GL_SILENCE_DEPRECATION
#endif
#include <GLFW/glfw3.h>

#if defined(_MSC_VER) && (_MSC_VER >= 1900) && !defined(IMGUI_DISABLE_WIN32_FUNCTIONS)
#pragma comment(lib, "legacy_stdio_definitions")
#endif

#include "lib/ImGuiFileDialog/ImGuiFileDialog.h"

//< ----------------imgui------------------ >
//< --------------------------------------- >





#define FILE_TYPE 0x4D42
#define FILE_HEADER_SIZE 14
#define INFO_HEADER_SIZE 40 
#define SPHERE_DIV_NUM 4



class FieldManager {

public:

  int readFlag = 0;
  
  int CELL_CAPACITY_OF_BLOCK;
  int FIELD_SIZE_X = 0;
  int FIELD_SIZE_Y = 0;
  int FIELD_SIZE_Z = 0; 
 
  struct Map {
    int wall;
    int NOC;
    double x, y, z;
    double cr, cg, cb;
    double ar, ag, ab;    
  };

  struct Block {
    int wall;
    int i, j, k;
    double x, y, z;
    double cr, cg, cb;
    double ar, ag, ab;    
  };
  
  Map*** Field;
  std::vector<Block> block_list;

  Block newBlock;
  int   newBlockFlag = -1;

  Block blockToBeDeleted;
  int blockToBeDeletedFlag = -1;

  ~FieldManager()
  {
    releaseField();
  }

  void releaseField()
  {
    for(int i=0; i<FIELD_SIZE_X; i++)
    {
      for(int j=0; j<FIELD_SIZE_Y; j++)
      {
	delete[] Field[i][j];
      }
      delete[] Field[i];
    }
    delete[] Field;
  }  

  void reset()
  {
    releaseField();
    //Field = nullptr;
    block_list.clear();
  }
  
  void read(std::string filename)
  {

    if(readFlag==1)
    {
      reset();
    }    
    
    std::ifstream ifs(filename);
    std::string line;
    
    {
      std::getline(ifs, line);
      std::istringstream iss_(line);
      iss_ >> FIELD_SIZE_X >> FIELD_SIZE_Y >> FIELD_SIZE_Z;
    }

    {
      std::getline(ifs, line);
      std::istringstream iss_(line);
      iss_ >> CELL_CAPACITY_OF_BLOCK;
    }

    Field = new Map**[FIELD_SIZE_X];
    for(int i=0; i<FIELD_SIZE_X; i++)
    {
      Field[i] = new Map*[FIELD_SIZE_Y];
      for(int j=0; j<FIELD_SIZE_Y; j++)
      {
	Field[i][j] = new Map[FIELD_SIZE_Z];
      }
    }

    for(int i=0; i<FIELD_SIZE_X; i++)
    {
      for(int j=0; j<FIELD_SIZE_Y; j++)
      {
	for(int k=0; k<FIELD_SIZE_Z; k++)
	{
	  std::getline(ifs, line);
	  std::istringstream iss0(line);
	  iss0 >> Field[i][j][k].wall;

	  std::getline(ifs, line);
	  std::istringstream iss1(line);
	  iss1 >> Field[i][j][k].NOC;

	  std::getline(ifs, line);
	  std::istringstream iss2(line);
	  double id;
	  for(int l=0; l<CELL_CAPACITY_OF_BLOCK; l++)
	  {
	    iss2 >> id;
	  }

	  std::getline(ifs, line);
	  std::istringstream iss3(line);
	  iss3 >> Field[i][j][k].x  >> Field[i][j][k].y  >> Field[i][j][k].z;

	  std::getline(ifs, line);
	  std::istringstream iss4(line);
	  iss4 >> Field[i][j][k].cr >> Field[i][j][k].cg >> Field[i][j][k].cb;

	  std::getline(ifs, line);
	  std::istringstream iss5(line);
	  iss5 >> Field[i][j][k].ar >> Field[i][j][k].ag >> Field[i][j][k].ab;

	  
	}
      }
    }


    for(int i=0; i<FIELD_SIZE_X; i++)
    {
      for(int j=0; j<FIELD_SIZE_Y; j++)
      {
	for(int k=0; k<FIELD_SIZE_Z; k++)
	{

	  if(Field[i][j][k].wall!=0)
	  {
	    std::cout << Field[i][j][k].wall << ", "
		      << Field[i][j][k].cr << ", "
		      << Field[i][j][k].cg << ", "
		      << Field[i][j][k].cb << std::endl;
	    block_list.push_back({Field[i][j][k].wall,
				  i, j, k, 
				  Field[i][j][k].x,  Field[i][j][k].y,  Field[i][j][k].z,
				  Field[i][j][k].cr, Field[i][j][k].cg, Field[i][j][k].cb,
				  Field[i][j][k].ar, Field[i][j][k].ag, Field[i][j][k].ab});
	  }

	}
      }
    }
    
    readFlag = 1;
    
  }

  void create_flat_field(int FIELD_SIZE_X_, int FIELD_SIZE_Y_, int FIELD_SIZE_Z_,
			 int CELL_CAPACITY_OF_BLOCK_)
  {

    if(readFlag==1)
    {
      reset();
    }

    FIELD_SIZE_X = FIELD_SIZE_X_;
    FIELD_SIZE_Y = FIELD_SIZE_Y_;
    FIELD_SIZE_Z = FIELD_SIZE_Z_;
    int FIELD_CENTER_X = 0;
    int FIELD_CENTER_Y = FIELD_SIZE_Y/4;
    int FIELD_CENTER_Z = 0;
    CELL_CAPACITY_OF_BLOCK = CELL_CAPACITY_OF_BLOCK_;

    Field = new Map**[FIELD_SIZE_X];
    for(int i=0; i<FIELD_SIZE_X; i++)
    {
      Field[i] = new Map*[FIELD_SIZE_Y];
      for(int j=0; j<FIELD_SIZE_Y; j++)
      {
	Field[i][j] = new Map[FIELD_SIZE_Z];
      }
    }

    for(int i=0; i<FIELD_SIZE_X; i++)
    {
      for(int j=0; j<FIELD_SIZE_Y; j++)
      {
	for(int k=0; k<FIELD_SIZE_Z; k++)
	{
	  Field[i][j][k].wall = 0;
	  Field[i][j][k].NOC = 0;

	  Field[i][j][k].x = round(i - FIELD_SIZE_X/2 + FIELD_CENTER_X);
	  Field[i][j][k].y = round(j - FIELD_SIZE_Y/2 + FIELD_CENTER_Y);
	  Field[i][j][k].z = round(k - FIELD_SIZE_Z/2 + FIELD_CENTER_Z);
	  
	  Field[i][j][k].cr = 0.0;
	  Field[i][j][k].cg = 0.0;
	  Field[i][j][k].cb = 0.0;

	  Field[i][j][k].ar = 0.5;
	  Field[i][j][k].ag = 0.5;
	  Field[i][j][k].ab = 0.5;

	  if(j==(FIELD_CENTER_Y-1))
	  {
	    Field[i][j][k].wall = 1;
	    
	    Field[i][j][k].cr = 0.5;
	    Field[i][j][k].cg = 0.5;
	    Field[i][j][k].cb = 0.5;

	    Field[i][j][k].ar = 0.5;
	    Field[i][j][k].ag = 0.5;
	    Field[i][j][k].ab = 0.5;
	  }

	  if(i==0 || i==(FIELD_SIZE_X-1))
	  {
	    Field[i][j][k].wall = 2;
	  }

	  if(j==0 || j==(FIELD_SIZE_Y-1))
	  {
	    Field[i][j][k].wall = 2;
	  }

	  if(k==0 || k==(FIELD_SIZE_Z-1))
	  {
	    Field[i][j][k].wall = 2;
	  }

	  if(Field[i][j][k].wall!=0)
	  {
	    std::cout << Field[i][j][k].wall << ", "
		      << Field[i][j][k].cr << ", "
		      << Field[i][j][k].cg << ", "
		      << Field[i][j][k].cb << std::endl;
	    block_list.push_back({Field[i][j][k].wall,
				  i, j, k, 
				  Field[i][j][k].x,  Field[i][j][k].y,  Field[i][j][k].z,
				  Field[i][j][k].cr, Field[i][j][k].cg, Field[i][j][k].cb,
				  Field[i][j][k].ar, Field[i][j][k].ag, Field[i][j][k].ab});
	  }
	  
	}
      }
    }

    readFlag = 1;
    
  }


  void create_random_field(int FIELD_SIZE_X_, int FIELD_SIZE_Y_, int FIELD_SIZE_Z_,
			   int CELL_CAPACITY_OF_BLOCK_)
  {
    
    int n, mx, my, rpx, rpy, dx, dy;
    double x, y, z, z00, z01, z10, z11, r, g, b;

    n = 4;
    
    if(readFlag==1)
    {
      reset();
    }

    FIELD_SIZE_X = FIELD_SIZE_X_;
    FIELD_SIZE_Y = FIELD_SIZE_Y_;
    FIELD_SIZE_Z = FIELD_SIZE_Z_;
    int FIELD_CENTER_X = 0;
    int FIELD_CENTER_Y = FIELD_SIZE_Y/4;
    int FIELD_CENTER_Z = 0;
    CELL_CAPACITY_OF_BLOCK = CELL_CAPACITY_OF_BLOCK_;

    Field = new Map**[FIELD_SIZE_X];
    for(int i=0; i<FIELD_SIZE_X; i++)
    {
      Field[i] = new Map*[FIELD_SIZE_Y];
      for(int j=0; j<FIELD_SIZE_Y; j++)
      {
	Field[i][j] = new Map[FIELD_SIZE_Z];
      }
    }

    for(int i=0; i<FIELD_SIZE_X; i++)
    {
      for(int j=0; j<FIELD_SIZE_Y; j++)
      {
	for(int k=0; k<FIELD_SIZE_Z; k++)
	{
	  Field[i][j][k].wall = 0;
	  Field[i][j][k].NOC = 0;

	  Field[i][j][k].x = round(i - FIELD_SIZE_X/2 + FIELD_CENTER_X);
	  Field[i][j][k].y = round(j - FIELD_SIZE_Y/2 + FIELD_CENTER_Y);
	  Field[i][j][k].z = round(k - FIELD_SIZE_Z/2 + FIELD_CENTER_Z);
	  
	  Field[i][j][k].cr = 0.0;
	  Field[i][j][k].cg = 0.0;
	  Field[i][j][k].cb = 0.0;

	  Field[i][j][k].ar = 0.5;
	  Field[i][j][k].ag = 0.5;
	  Field[i][j][k].ab = 0.5;
	}
      }
    }
    
    mx = round(log(FIELD_SIZE_X) / log(2.0));
    my = round(log(FIELD_SIZE_Z) / log(2.0));

    rpx = pow(2, mx-n) + 1;
    rpy = pow(2, my-n) + 1;

    std::vector<std::vector<double>> rfieldx(rpx, std::vector<double>(rpy));
    std::vector<std::vector<double>> rfieldy(rpx, std::vector<double>(rpy));
    
    dx = pow(2, n);
    dy = pow(2, n);

    for(int i=0; i<rpx; i++)
    {
      for(int j=0; j<rpy; j++)
      {
	rfieldx[i][j] = rand() / (RAND_MAX + 1.0) - 0.5;
	rfieldy[i][j] = rand() / (RAND_MAX + 1.0) - 0.5;
      }
    }
    
    for(int i=1; i<FIELD_SIZE_X; i++)
    {
      mx = i/dx;
      for(int j=1; j<FIELD_SIZE_Z; j++)
      {
	my = j/dy;

	x = (double)(i - dx*(mx + 1));
	y = (double)(j - dy*(my + 1));
	//z11 = x*rfieldx[mx + 2][my + 2] + y*rfieldy[mx + 2][my + 2];
	z11 = x*rfieldx[mx+1][my+1] + y*rfieldy[mx+1][my+1];
		
	x = (double)(i - dx*mx);
	y = (double)(j - dy*(my + 1));
	//z01 = x*rfieldx[mx + 1][my + 2] + y*rfieldy[mx + 1][my + 2];
	z01 = x*rfieldx[mx][my+1] + y*rfieldy[mx][my+1];
	
	x = (double)(i - dx*(mx + 1));
	y = (double)(j - dy*my);
	//z10 = x*rfieldx[mx + 2][my + 1] + y*rfieldy[mx + 2][my + 1];
	z10 = x*rfieldx[mx+1][my] + y*rfieldy[mx+1][my];
	
	x = (double)(i - dx*mx);
	y = (double)(j - dy*my);
	//z00 = x*rfieldx[mx + 1][my + 1] + y*rfieldy[mx + 1][my + 1];
	z00 = x*rfieldx[mx][my] + y*rfieldy[mx][my];
	
	x = x/double(dx);
	y = y/double(dy);
	
	x = 6.0*pow(x,5) - 15.0*pow(x,4) + 10.0*pow(x,3);
	y = 6.0*pow(y,5) - 15.0*pow(y,4) + 10.0*pow(y,3);
	
	
	z = (1.0 - x) * (1.0 - y) * z00;
	z += (1.0 - x) * y * z01;
	z += x * (1.0 - y) * z10;
	z += x * y * z11;
	
	if (round(z) + FIELD_CENTER_Y <= FIELD_CENTER_Y * 0.94)
	{
	  r = 0.110;
	  g = 0.020;
	  b = 1.0;
	}
	else if (FIELD_CENTER_Y * 0.94 < round(z) + FIELD_CENTER_Y
		 && round(z) + FIELD_CENTER_Y <= FIELD_CENTER_Y)
	{
	  r = 0.522;
	  g = 0.478;
	  b = 0.235;
	}
	else
	{
	  r = 0.133;
	  g = 0.765;
	  b = 0.314;
	}

	int k = round(z) + FIELD_SIZE_Y/2 - FIELD_CENTER_Y;
	Field[i-1][k][j-1].wall = 1;
	Field[i-1][k][j-1].cr = r;
	Field[i-1][k][j-1].cg = g;
	Field[i-1][k][j-1].cb = b;

      }
    }

    for(int i=0; i<FIELD_SIZE_X; i++)
    {
      for(int j=0; j<FIELD_SIZE_Y; j++)
      {
	for(int k=0; k<FIELD_SIZE_Z; k++)
	{

	  if(i==0 || i==(FIELD_SIZE_X-1))
	  {
	    Field[i][j][k].wall = 2;
	  }

	  if(j==0 || j==(FIELD_SIZE_Y-1))
	  {
	    Field[i][j][k].wall = 2;
	  }

	  if(k==0 || k==(FIELD_SIZE_Z-1))
	  {
	    Field[i][j][k].wall = 2;
	  }

	}
      }
    }
    
    for(int i=0; i<FIELD_SIZE_X; i++)
    {
      for(int j=0; j<FIELD_SIZE_Y; j++)
      {
	for(int k=0; k<FIELD_SIZE_Z; k++)
	{
	  
	  if(Field[i][j][k].wall!=0)
	  {
	    std::cout << Field[i][j][k].wall << ", "
		      << Field[i][j][k].cr << ", "
		      << Field[i][j][k].cg << ", "
		      << Field[i][j][k].cb << std::endl;
	    block_list.push_back({Field[i][j][k].wall,
				  i, j, k, 
				  Field[i][j][k].x,  Field[i][j][k].y,  Field[i][j][k].z,
				  Field[i][j][k].cr, Field[i][j][k].cg, Field[i][j][k].cb,
				  Field[i][j][k].ar, Field[i][j][k].ag, Field[i][j][k].ab});
	  }

	}
      }
    }
    
    readFlag = 1;

    
  }

















  
  void write(std::string filename)
  {

    std::ofstream newFieldFile(filename, std::ios::out);

    newFieldFile << FIELD_SIZE_X << " " << FIELD_SIZE_Y << " " << FIELD_SIZE_Z << " \n";
    newFieldFile << CELL_CAPACITY_OF_BLOCK << " \n";    

    for(int i=0; i<FIELD_SIZE_X; i++)
    {
      for(int j=0; j<FIELD_SIZE_Y; j++)
      {
	for(int k=0; k<FIELD_SIZE_Z; k++)
	{
	  newFieldFile << Field[i][j][k].wall << " \n";
	  newFieldFile << Field[i][j][k].NOC  << " \n";
	  
	  for(int l=0; l<CELL_CAPACITY_OF_BLOCK; l++)
	  {
	    newFieldFile << "0 ";
	  }
	  newFieldFile << "\n";

	  newFieldFile << Field[i][j][k].x  << " " << Field[i][j][k].y  << " " << Field[i][j][k].z  << " \n";
	  newFieldFile << Field[i][j][k].cr << " " << Field[i][j][k].cg << " " << Field[i][j][k].cb << " \n";
	  newFieldFile << Field[i][j][k].ar << " " << Field[i][j][k].ag << " " << Field[i][j][k].ab << " \n";
	  
	}
      }
    }

  }
  
  void detectNewCandidateBlock(double ex, double ey, double ez, double* viewVec)
  {
    int w;
    int xi, yi, zi;
    double x, y, z;
    double vl, d_min;
    double det;
    double v0, v1, v2;
    double a0, a1, a2;
    double b0, b1, b2;
    double c0, c1, c2;
    double m00, m01, m02;
    double m10, m11, m12;
    double m20, m21, m22;
    double s, t, l;
    int length;

    double oVec[6][3] = {{ 0.5, -0.5, -0.5},
			 { 0.5, -0.5,  0.5},
			 {-0.5, -0.5,  0.5},
			 {-0.5, -0.5, -0.5},
			 {-0.5,  0.5, -0.5},
			 {-0.5, -0.5, -0.5}};
    double aVec[6][3] = {{ 0.0,  0.0,  1.0},
			 {-1.0,  0.0,  0.0},
			 { 0.0,  0.0, -1.0},
			 { 1.0,  0.0,  0.0},
			 { 1.0,  0.0,  0.0},
			 { 1.0,  0.0,  0.0}};
    double bVec[6][3] = {{ 0.0,  1.0,  0.0},
			 { 0.0,  1.0,  0.0},
			 { 0.0,  1.0,  0.0},
			 { 0.0,  1.0,  0.0},
			 { 0.0,  0.0,  1.0},
			 { 0.0,  0.0,  1.0}};
    double nVec[6][3] = {{ 1.0,  0.0,  0.0},
			 { 0.0,  0.0,  1.0},
			 {-1.0,  0.0,  0.0},
			 { 0.0,  0.0, -1.0},
			 { 0.0,  1.0,  0.0},
			 { 0.0, -1.0,  0.0}};
    
    vl = sqrt(viewVec[0]*viewVec[0] + viewVec[1]*viewVec[1] + viewVec[2]*viewVec[2]);
    v0 = viewVec[0]/vl;
    v1 = viewVec[1]/vl;
    v2 = viewVec[2]/vl;

    d_min = DBL_MAX;
    newBlockFlag = -1;

    length = block_list.size();
    for(int bc=0; bc<length; bc++)
    {

      w  = block_list[bc].wall;
      xi = block_list[bc].i;
      yi = block_list[bc].j;
      zi = block_list[bc].k;
      x  = block_list[bc].x;
      y  = block_list[bc].y;
      z  = block_list[bc].z;

      if(w!=1)
      {
	continue;
      }
      
      for(int si=0; si<6; si++)
      {
	a0 = aVec[si][0];
	a1 = aVec[si][1];
	a2 = aVec[si][2];
	b0 = bVec[si][0];
	b1 = bVec[si][1];
	b2 = bVec[si][2];
	det = a0*b1*v2 + b0*v1*a2 + v0*a1*b2;
	det = det - (v0*b1*a2 + v1*b2*a0 + v2*b0*a1);
	if(det==0.0)
	{
	  continue;
	}
	m00 =  (b1*v2 - v1*b2)/det;
	m01 = -(b0*v2 - v0*b2)/det;
	m02 =  (b0*v1 - v0*b1)/det;
	m10 = -(a1*v2 - v1*a2)/det;
	m11 =  (a0*v2 - v0*a2)/det;
	m12 = -(a0*v1 - v0*a1)/det;
	m20 =  (a1*b2 - b1*a2)/det;
	m21 = -(a0*b2 - b0*a2)/det;
	m22 =  (a0*b1 - b0*a1)/det;
	
	c0  = ex - (x + oVec[si][0]);
	c1  = ey - (y + oVec[si][1]);
	c2  = ez - (z + oVec[si][2]);
	
	s = m00*c0 + m01*c1 + m02*c2;
	t = m10*c0 + m11*c1 + m12*c2;
	l = m20*c0 + m21*c1 + m22*c2;
	
	l = -l;

	if(l<0.0)
	{
	  continue;
	}

	if(s<0.0 || 1.0<s)
	{
	  continue;
	}

	if(t<0.0 || 1.0<t)
	{
	  continue;
	}
	
	if(l<d_min)
	{
	  d_min = l;
	  newBlockFlag = 1;
	  newBlock.wall = 1;
	  newBlock.i = xi + round(nVec[si][0]);
	  newBlock.j = yi + round(nVec[si][1]);
	  newBlock.k = zi + round(nVec[si][2]);
	  newBlock.x = x + nVec[si][0];
	  newBlock.y = y + nVec[si][1];
	  newBlock.z = z + nVec[si][2];
	  newBlock.cr = 0.5;
	  newBlock.cg = 0.5;
	  newBlock.cb = 0.5;
	  newBlock.ar = 0.5;
	  newBlock.ag = 0.5;
	  newBlock.ab = 0.5;  
	}
	
      }
    }
    
  }

  void detectBlockToBeDeleted(double ex, double ey, double ez, double* viewVec)
  {
    int w;
    int xi, yi, zi;
    double x, y, z;
    double vl, d_min;
    double det;
    double v0, v1, v2;
    double a0, a1, a2;
    double b0, b1, b2;
    double c0, c1, c2;
    double m00, m01, m02;
    double m10, m11, m12;
    double m20, m21, m22;
    double s, t, l;
    int length;

    double oVec[6][3] = {{ 0.5, -0.5, -0.5},
			  { 0.5, -0.5,  0.5},
			  {-0.5, -0.5,  0.5},
			  {-0.5, -0.5, -0.5},
			  {-0.5,  0.5, -0.5},
			  {-0.5, -0.5, -0.5}};
    double aVec[6][3] = {{ 0.0,  0.0,  1.0},
			  {-1.0,  0.0,  0.0},
			  { 0.0,  0.0, -1.0},
			  { 1.0,  0.0,  0.0},
			  { 1.0,  0.0,  0.0},
			  { 1.0,  0.0,  0.0}};
    double bVec[6][3] = {{ 0.0,  1.0,  0.0},
			  { 0.0,  1.0,  0.0},
			  { 0.0,  1.0,  0.0},
			  { 0.0,  1.0,  0.0},
			  { 0.0,  0.0,  1.0},
			  { 0.0,  0.0,  1.0}};
    
    vl = sqrt(viewVec[0]*viewVec[0] + viewVec[1]*viewVec[1] + viewVec[2]*viewVec[2]);
    v0 = viewVec[0]/vl;
    v1 = viewVec[1]/vl;
    v2 = viewVec[2]/vl;

    d_min = DBL_MAX;
    blockToBeDeletedFlag = -1;

    length = block_list.size();
    for(int bc=0; bc<length; bc++)
    {

      w  = block_list[bc].wall;
      xi = block_list[bc].i;
      yi = block_list[bc].j;
      zi = block_list[bc].k;
      x  = block_list[bc].x;
      y  = block_list[bc].y;
      z  = block_list[bc].z;

      if(w!=1)
      {
	continue;
      }
      
      for(int si=0; si<6; si++)
      {
	a0 = aVec[si][0];
	a1 = aVec[si][1];
	a2 = aVec[si][2];
	b0 = bVec[si][0];
	b1 = bVec[si][1];
	b2 = bVec[si][2];
	det = a0*b1*v2 + b0*v1*a2 + v0*a1*b2;
	det = det - (v0*b1*a2 + v1*b2*a0 + v2*b0*a1);
	if(det==0.0)
	{
	  continue;
	}
	m00 =  (b1*v2 - v1*b2)/det;
	m01 = -(b0*v2 - v0*b2)/det;
	m02 =  (b0*v1 - v0*b1)/det;
	m10 = -(a1*v2 - v1*a2)/det;
	m11 =  (a0*v2 - v0*a2)/det;
	m12 = -(a0*v1 - v0*a1)/det;
	m20 =  (a1*b2 - b1*a2)/det;
	m21 = -(a0*b2 - b0*a2)/det;
	m22 =  (a0*b1 - b0*a1)/det;
	
	c0  = ex - (x + oVec[si][0]);
	c1  = ey - (y + oVec[si][1]);
	c2  = ez - (z + oVec[si][2]);
	
	s = m00*c0 + m01*c1 + m02*c2;
	t = m10*c0 + m11*c1 + m12*c2;
	l = m20*c0 + m21*c1 + m22*c2;
	
	l = -l;

	if(l<0.0)
	{
	  continue;
	}

	if(s<0.0 || 1.0<s)
	{
	  continue;
	}

	if(t<0.0 || 1.0<t)
	{
	  continue;
	}
	
	if(l<d_min)
	{
	  d_min = l;
	  blockToBeDeletedFlag = 1;
	  blockToBeDeleted.wall = 0;
	  blockToBeDeleted.i = xi;
	  blockToBeDeleted.j = yi;
	  blockToBeDeleted.k = zi;
	  blockToBeDeleted.x = x;
	  blockToBeDeleted.y = y;
	  blockToBeDeleted.z = z;
	  blockToBeDeleted.cr = 0.0;
	  blockToBeDeleted.cg = 0.0;
	  blockToBeDeleted.cb = 0.0;
	  blockToBeDeleted.ar = 0.0;
	  blockToBeDeleted.ag = 0.0;
	  blockToBeDeleted.ab = 0.0;  
	}
	
      }
    }
    
  }
  
  void addBlock()
  {

    if(Field[newBlock.i][newBlock.j][newBlock.k].wall==0)
    {
      Field[newBlock.i][newBlock.j][newBlock.k].wall = newBlock.wall;
      Field[newBlock.i][newBlock.j][newBlock.k].x    = newBlock.x;
      Field[newBlock.i][newBlock.j][newBlock.k].y    = newBlock.y;
      Field[newBlock.i][newBlock.j][newBlock.k].z    = newBlock.z;
      Field[newBlock.i][newBlock.j][newBlock.k].cr   = newBlock.cr;
      Field[newBlock.i][newBlock.j][newBlock.k].cg   = newBlock.cg;
      Field[newBlock.i][newBlock.j][newBlock.k].cb   = newBlock.cb;
      Field[newBlock.i][newBlock.j][newBlock.k].ar   = newBlock.ar;
      Field[newBlock.i][newBlock.j][newBlock.k].ag   = newBlock.ag;
      Field[newBlock.i][newBlock.j][newBlock.k].ab   = newBlock.ab;
      block_list.push_back({newBlock.wall,
			    newBlock.i,  newBlock.j,  newBlock.k,
			    newBlock.x,  newBlock.y,  newBlock.z,
			    newBlock.cr, newBlock.cg, newBlock.cb,
			    newBlock.ar, newBlock.ag, newBlock.ab});
    }
    newBlockFlag = -1;
  }

  void removeBlock()
  {
    for(long unsigned int c=0; c<block_list.size(); c++)
    {
      if(block_list[c].i==blockToBeDeleted.i
	 && block_list[c].j==blockToBeDeleted.j
	 && block_list[c].k==blockToBeDeleted.k)
      {
	block_list.erase(block_list.begin() + c);
	break;
      }
    }

    Field[blockToBeDeleted.i][blockToBeDeleted.j][blockToBeDeleted.k].wall = blockToBeDeleted.wall;
    Field[blockToBeDeleted.i][blockToBeDeleted.j][blockToBeDeleted.k].x    = blockToBeDeleted.x;
    Field[blockToBeDeleted.i][blockToBeDeleted.j][blockToBeDeleted.k].y    = blockToBeDeleted.y;
    Field[blockToBeDeleted.i][blockToBeDeleted.j][blockToBeDeleted.k].z    = blockToBeDeleted.z;
    Field[blockToBeDeleted.i][blockToBeDeleted.j][blockToBeDeleted.k].cr   = blockToBeDeleted.cr;
    Field[blockToBeDeleted.i][blockToBeDeleted.j][blockToBeDeleted.k].cg   = blockToBeDeleted.cg;
    Field[blockToBeDeleted.i][blockToBeDeleted.j][blockToBeDeleted.k].cb   = blockToBeDeleted.cb;
    Field[blockToBeDeleted.i][blockToBeDeleted.j][blockToBeDeleted.k].ar   = blockToBeDeleted.ar;
    Field[blockToBeDeleted.i][blockToBeDeleted.j][blockToBeDeleted.k].ag   = blockToBeDeleted.ag;
    Field[blockToBeDeleted.i][blockToBeDeleted.j][blockToBeDeleted.k].ab   = blockToBeDeleted.ab;

    blockToBeDeletedFlag = -1;
    
  }

  
};


class Cell {

public:

  Cell()
  {
    int i=1;
    for (char c : CODES) {
      CODES_dic[c] = i;
      i++;
    }
  }

  std::string CODES = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+-";
  std::unordered_map<char, int> CODES_dic;

  int NOIU;
  int NOOU;

  std::vector<std::string> rawData;
  std::map<std::string, int> rawIndex;
  
  int exist;

  std::vector<int> book;
  std::vector<int> bookmarker;
  std::vector<int> bookmarker_advance;
    
  double x, y, z;
  double ox, oy, oz;
  int nx, ny, nz;
  double vx, vy, vz;
  double fx, fy, fz;
  double cr, cg, cb;
  double ar, ag, ab;
  double IOLr;  //Intensity Of Light r
  double IOLg;  //Intensity Of Light g
  double IOLb;  //Intensity Of Light b
  double m;
  double r;
  double E;
  double alpha;

  int ID;
  int64_t age;

  std::vector<double> in_data;
  std::vector<double> out_data;
  std::vector<std::vector<double>> w1;
  std::vector<std::vector<double>> w2;

  std::vector<double> KOCC; //K Of Connected Cell
  std::vector<double> LOCC; //L Of Connected Cell
  std::vector<double> SOCC; //S Of Connected Cell

  std::vector<int> CCF;   //Connected Cell Flag
  std::vector<int> IOCC;  //Id Of Connected Cell
  std::vector<int> UIOCC; //Unit Id Of Connected Cell

  int HIT_BLOCK_AF;
  int HIT_CELL_AF;
  int SPRING_AF;
  int MECHANICS_AF;

  int EAT_AF;
  int FUSION_AF;
  int LIGHT_AF;

  int INFO_TRANS_F;
  int NEURAL_NETWORK_F;

  int WAIT_FOR_CONNECT_F;
  int WAIT_FOR_CONNECT_UI;
  int WAIT_FOR_DISCONNECT_F;

  int ALONE_F;

  void readFromLines(std::vector<std::string>& lines, int rli)
  {
    
    int linesLen = static_cast<int>(lines.size());
    
    for(int li=0; li<linesLen; li++)
    {
      std::string line = lines[rli+li];
      
      if(li==0)
      {
	rawData.push_back(line);
	rawIndex["exist"] = li;
	exist = std::stoi(line);
      }
      else if(li==1)
      {
	rawData.push_back(line);
	rawIndex["book"] = li;
	line.erase(0, 1);
	if(!line.empty())
	{
	  for (char c : line) {
	    book.push_back(CODES_dic[c]);
	  }
	}
      }
      else if(li==2)
      {
	rawData.push_back(line);
	rawIndex["bookmarker"] = li;
	line.erase(0, 1);
	if(!line.empty())
	{
	  for (char c : line) {
	    bookmarker.push_back(CODES_dic[c]);
	  }
	}
      }
      else if(li==3)
      {
	rawData.push_back(line);
	rawIndex["bookmarker_advance"] = li;
	line.erase(0, 1);
	if(!line.empty())
	{
	  for (char c : line) {
	    bookmarker_advance.push_back(CODES_dic[c]);
	  }
	}
      }
      else if(li==4)
      {
	rawData.push_back(line);
	rawIndex["xyz"] = li;
	std::istringstream iss(line);
	iss >> x >> y >> z;
	ox = x;
	oy = y;
	oz = z;
      }
      else if(li==5)
      {
	rawData.push_back(line);
	rawIndex["nxnynz"] = li;
	std::istringstream iss(line);
	iss >> nx >> ny >> nz;
      }
      else if(li==6)
      {
	rawData.push_back(line);
	rawIndex["vxvyvz"] = li;
	std::istringstream iss(line);
	iss >> vx >> vy >> vz;
      }
      else if(li==7)
      {
	rawData.push_back(line);
	rawIndex["fxfyfz"] = li;
	std::istringstream iss(line);
	iss >> fx >> fy >> fz;
      }
      else if(li==8)
      {
	rawData.push_back(line);
	rawIndex["crcgcb"] = li;
	std::istringstream iss(line);
	iss >> cr >> cg >> cb;
      }
      else if(li==9)
      {
	rawData.push_back(line);
	rawIndex["aragab"] = li;
	std::istringstream iss(line);
	iss >> ar >> ag >> ab;
      }
      else if(li==10)
      {
	rawData.push_back(line);
	rawIndex["IOLr"] = li;
	std::istringstream iss(line);
	iss >> IOLr;
      }
      else if(li==11)
      {
	rawData.push_back(line);
	rawIndex["IOLg"] = li;
	std::istringstream iss(line);
	iss >> IOLg;
      }
      else if(li==12)
      {
	rawData.push_back(line);
	rawIndex["IOLb"] = li;
	std::istringstream iss(line);
	iss >> IOLb;
      }
      else if(li==13)
      {
	rawData.push_back(line);
	rawIndex["m"] = li;
	std::istringstream iss(line);
	iss >> m;
      }
      else if(li==14)
      {
	rawData.push_back(line);
	rawIndex["r"] = li;
	std::istringstream iss(line);
	iss >> r;
      }
      else if(li==15)
      {
	rawData.push_back(line);
	rawIndex["E"] = li;
	std::istringstream iss(line);
	iss >> E;
      }
      else if(li==16)
      {
	rawData.push_back(line);
	rawIndex["alpha"] = li;
	std::istringstream iss(line);
	iss >> alpha;
      }
      else if(li==17)
      {
	rawData.push_back(line);
	rawIndex["ID"] = li;
	std::istringstream iss(line);
	iss >> ID;
      }
      else if(li==18)
      {
	rawData.push_back(line);
	rawIndex["age"] = li;
	std::istringstream iss(line);
	iss >> age;
      }
      else if(li==19)
      {
	rawData.push_back(line);
	rawIndex["in_data"] = li;
	std::istringstream iss(line);
	double val;
	while (iss >> val)
	{
	  in_data.push_back(val);
	}
	NOIU = in_data.size();
      }
      else if(li==20)
      {
	rawData.push_back(line);
	rawIndex["out_data"] = li;
	std::istringstream iss(line);
	double val;
	while (iss >> val)
	{
	  out_data.push_back(val);
	}
	NOOU = out_data.size();
      }
      else if(li==21)
      {
	rawData.push_back(line);
	rawIndex["w1"] = li;
	
	std::istringstream stream(line);
	std::vector<double> vec;
	  
	while (stream >> std::ws) {
	  double token;
	  stream >> token;
	  vec.push_back(token);
	}

	int len = vec.size();
	
	int cn = NOIU+1;
	int rn = len/cn;
	std::vector<std::vector<double>> tmp(cn, std::vector<double>(rn));

	int c=0;
	for (int i=0; i<cn; i++) {
	  std::vector<double> row;
	  for (int j=0; j<rn; j++) {
	    row.push_back(vec[c]);
	    c++;
	  }
	  tmp.push_back(row);
	}
	
	for (int i=0; i<rn; i++) {
	  std::vector<double> row;
	  for (int j=0; j<cn; j++) {
	    row.push_back(tmp[j][i]);
	  }
	  w1.push_back(row);
	}

      }
      else if(li==22)
      {
	rawData.push_back(line);
	rawIndex["w2"] = li;
	
	std::istringstream stream(line);
	std::vector<double> vec;
	  
	while (stream >> std::ws) {
	  double token;
	  stream >> token;
	  vec.push_back(token);
	}

	int len = vec.size();

	int rn = NOOU;
	int cn = len/rn;
	std::vector<std::vector<double>> tmp(cn, std::vector<double>(rn));

	int c=0;
	for (int i=0; i<cn; i++) {
	  std::vector<double> row;
	  for (int j=0; j<rn; j++) {
	    row.push_back(vec[c]);
	    c++;
	  }
	  tmp.push_back(row);
	}
	
	for (int i=0; i<rn; i++) {
	  std::vector<double> row;
	  for (int j=0; j<cn; j++) {
	    row.push_back(tmp[j][i]);
	  }
	  w2.push_back(row);
	}
	
      }
      else if(li==23)
      {
	rawData.push_back(line);
	rawIndex["KOCC"] = li;
	std::istringstream iss(line);
	double val;
	while (iss >> val)
	{
	  KOCC.push_back(val);
	}
      }
      else if(li==24)
      {
	rawData.push_back(line);
	rawIndex["LOCC"] = li;
	std::istringstream iss(line);
	double val;
	while (iss >> val)
	{
	  LOCC.push_back(val);
	}
      }
      else if(li==25)
      {
	rawData.push_back(line);
	rawIndex["SOCC"] = li;
	std::istringstream iss(line);
	double val;
	while (iss >> val)
	{
	  SOCC.push_back(val);
	}
      }
      else if(li==26)
      {
	rawData.push_back(line);
	rawIndex["CCF"] = li;
	std::istringstream iss(line);
	double val;
	while (iss >> val)
	{
	  CCF.push_back(val);
	}
      }
      else if(li==27)
      {
	rawData.push_back(line);
	rawIndex["IOCC"] = li;
	std::istringstream iss(line);
	double val;
	while (iss >> val)
	{
	  IOCC.push_back(val);
	}
      }
      else if(li==28)
      {
	rawData.push_back(line);
	rawIndex["UIOCC"] = li;
	std::istringstream iss(line);
	double val;
	while (iss >> val)
	{
	  UIOCC.push_back(val);
	}
      }
      else if(li==29)
      {
	rawData.push_back(line);
	rawIndex["HIT_BLOCK_AF"] = li;
	HIT_BLOCK_AF = std::stoi(line);
      }
      else if(li==30)
      {
	rawData.push_back(line);
	rawIndex["HIT_CELL_AF"] = li;
	HIT_CELL_AF = std::stoi(line);
      }
      else if(li==31)
      {
	rawData.push_back(line);
	rawIndex["SPRING_AF"] = li;
	SPRING_AF = std::stoi(line);
      }
      else if(li==32)
      {
	rawData.push_back(line);
	rawIndex["MECHANICS_AF"] = li;
	MECHANICS_AF = std::stoi(line);
      }
      else if(li==33)
      {
	rawData.push_back(line);
	rawIndex["EAT_AF"] = li;
	EAT_AF = std::stoi(line);
      }
      else if(li==34)
      {
	rawData.push_back(line);
	rawIndex["FUSION_AF"] = li;
	FUSION_AF = std::stoi(line);
      }
      else if(li==35)
      {
	rawData.push_back(line);
	rawIndex["LIGHT_AF"] = li;
	LIGHT_AF = std::stoi(line);
      }
      else if(li==36)
      {
	rawData.push_back(line);
	rawIndex["INFO_TRANS_F"] = li;
	INFO_TRANS_F = std::stoi(line);
      }
      else if(li==37)
      {
	rawData.push_back(line);
	rawIndex["NEURAL_NETWORK_F"] = li;
	NEURAL_NETWORK_F = std::stoi(line);
      }
      else if(li==38)
      {
	rawData.push_back(line);
	rawIndex["WAIT_FOR_CONNECT_F"] = li;
	WAIT_FOR_CONNECT_F = std::stoi(line);
      }
      else if(li==39)
      {
	rawData.push_back(line);
	rawIndex["WAIT_FOR_CONNECT_UI"] = li;
	WAIT_FOR_CONNECT_UI = std::stoi(line);
      }
      else if(li==40)
      {
	rawData.push_back(line);
	rawIndex["WAIT_FOR_DISCONNECT_F"] = li;
	WAIT_FOR_DISCONNECT_F = std::stoi(line);
      }
      else if(li==41)
      {
	rawData.push_back(line);
	rawIndex["ALONE_F"] = li;
	ALONE_F = std::stoi(line);
	break;
      }
      else if(li==42)
      {
	break;
      }

      
    }
  }
  
};

class CellManager{

public:

  int sunId = 1;
  int sunIndex = 0;
  int oneCellLen = 42;
  int firstReadFlag = 1;
  std::vector<Cell> cells;
  std::vector<int> groupList;
  std::vector<int> aliveFlagList;
  int groupCount = 0;
  std::vector<std::vector<int>> kList;
  int num = 0;
  //std::map<int, int> idDict;
  std::map<int, std::map<int, int>> idDict;
  std::vector<int> selectedCellIndices;
  int lastSelectedCellIndex=-1;

  double gx, gy, gz;
  double prevTranslateX = 0.0;
  double prevTranslateY = 0.0;
  double prevTranslateZ = 0.0;

  
  void addCell(Cell c, int gi)
  {
    cells.push_back(c);
    groupList.push_back(gi);
    aliveFlagList.push_back(1);
  }

  void readFile(std::string filename)
  {
    std::ifstream file(filename);
    std::string line;
    std::vector<std::string> lines;

    while (std::getline(file, line)) {
      lines.push_back(line);
    }

    int linesLen = static_cast<int>(lines.size());
    int rli = 1;
    
    while(true)
    {
      
      std::cout << linesLen << ", " << rli << std::endl;

      if((linesLen-rli)<oneCellLen) break;

      Cell c;
      c.readFromLines(lines, rli);
      std::cout << c.ID << ", " << c.x << ", " << c.y << ", " << c.z << std::endl;
      
      if(c.exist==1)
      {
	if(firstReadFlag==1 || c.ID!=sunId)
	{
	  idDict[groupCount][c.ID] = num;
	  addCell(c, groupCount);
	  if(c.ID==sunId)
	  {
	    sunIndex = num;
	  }
	  num++;
	}
      }

      rli += oneCellLen;
      
    }
    
    firstReadFlag = 0;
    groupCount++;
    setKList();
        
  }

  void setKList()
  {
    kList = std::vector<std::vector<int>>();
    for(int i=0; i<num; i++)
    {
      int id1 = cells[i].ID;
      int noc = cells[i].CCF.size();
      for(int ci=0; ci<noc; ci++)
      {
	if(cells[i].CCF[ci]==1)
	{
	  int id2 = cells[i].IOCC[ci];
	  int i1  = idDict[groupList[i]][id1];
	  int i2  = idDict[groupList[i]][id2];
	  if(aliveFlagList[i1]==0 || aliveFlagList[i2]==0)
	  {
	    continue;
	  }
	  if(i1<i2)
	  {
	    std::vector<int> pair(2);
	    pair[0] = i1;
	    pair[1] = i2;
	    kList.push_back(pair);
	  }
	}
      }
    }
  }

  int getIntersectedCellIndex(double ex, double ey, double ez, double* viewVec, int SHOW_IT_FLAG)
  {
    int tindex=-1;
    double x, y, z;
    double dx, dy, dz;
    double vl, d, d_min;
    double v[3];
    double p[3];
    
    vl   = sqrt(viewVec[0]*viewVec[0] + viewVec[1]*viewVec[1] + viewVec[2]*viewVec[2]);
    v[0] = viewVec[0]/vl;
    v[1] = viewVec[1]/vl;
    v[2] = viewVec[2]/vl;
    
    d_min = DBL_MAX;
    
    for(int index=0; index<num; index++)
    {

      if(aliveFlagList[index]==0)
      {
	continue;
      }
      
      if(SHOW_IT_FLAG==0 && cells[index].INFO_TRANS_F==1)
      {
	continue;
      }
      
      x = cells[index].x;
      y = cells[index].y;
      z = cells[index].z;
      vl = (x - ex)*v[0] + (y - ey)*v[1] + (z - ez)*v[2];
      p[0] = ex + vl*v[0];
      p[1] = ey + vl*v[1];
      p[2] = ez + vl*v[2];
      dx   = x - p[0];
      dy   = y - p[1];
      dz   = z - p[2];
      d    = sqrt(dx*dx+dy*dy+dz*dz);
      if(d<cells[index].r)
      {
	dx   = p[0] - ex;
	dy   = p[1] - ey;
	dz   = p[2] - ez;
	d    = sqrt(dx*dx+dy*dy+dz*dz);
	if(d<d_min)
	{
	  d_min = d;
	  tindex   = index;
	}
      }
    }

    return(tindex);
    
  }

  void updateSelectedCellInfo()
  {
    gx = 0.0;
    gy = 0.0;
    gz = 0.0;
    for(long unsigned int i=0; i<selectedCellIndices.size(); i++)
    {
      gx += cells[i].x;
      gy += cells[i].y;
      gz += cells[i].z;
    }
    double n = static_cast<double>(selectedCellIndices.size());
    gx = gx/n;
    gy = gy/n;
    gz = gz/n;
    prevTranslateX = 0.0;
    prevTranslateY = 0.0;
    prevTranslateZ = 0.0;
  }
  
  void resetSelectedCellIndices()
  {
    selectedCellIndices = std::vector<int>();
    lastSelectedCellIndex = -1;
  }
  
  void addSelectedCell(int i)
  {
    if(sunIndex==i)
    {
      return;
    }
    
    if(std::find(selectedCellIndices.begin(), selectedCellIndices.end(), i)==selectedCellIndices.end())
    {
      selectedCellIndices.push_back(i);
    }
    lastSelectedCellIndex = i;
    updateSelectedCellInfo();
  }

  void removeSelectedCell(int i)
  {
    selectedCellIndices.erase(
			      std::remove(selectedCellIndices.begin(),
					  selectedCellIndices.end(),
					  i),
			      selectedCellIndices.end());
    updateSelectedCellInfo();
  }

  

  void killSelectedCell()
  {
    for(long unsigned int i=0; i<selectedCellIndices.size(); i++)
    {
      aliveFlagList[selectedCellIndices[i]] = 0;
    }
    resetSelectedCellIndices();
    setKList();
  }
  
  void translateSelectedCell(double x, double y, double z)
  {

    double dx = x - prevTranslateX;
    double dy = y - prevTranslateY;
    double dz = z - prevTranslateZ;

    prevTranslateX = x;
    prevTranslateY = y;
    prevTranslateZ = z;
    
    for(long unsigned int i=0; i<selectedCellIndices.size(); i++)
    {
      int index = selectedCellIndices[i];
      cells[index].x += dx;
      cells[index].y += dy;
      cells[index].z += dz;
    }
  }

  void selectConnectedCell(int index)
  {
    int group = 0;
    int connectedCount = 1;
    int preConnectedCount = 1;
    std::unordered_map<int, int> id2indexDic;
    std::unordered_map<int, int> countDic;
    std::unordered_map<int, int> countDic_;

    if(index<0)
    {
      return;
    }

    group = groupList[index];
    
    for(long unsigned int i=0; i<cells.size(); i++)
    {
      if(groupList[i]==group)
      {
	id2indexDic[cells[i].ID] = i;
	countDic[i] = 0;
	countDic_[i] = 0;
      }      
    }

    countDic[index] = 1;
    countDic_[index] = 1;
    
    while(true)
    {

      for (const auto& entry : id2indexDic)
      {
	int i  = entry.second;
	if(0<countDic[i])
	{
	  for(long unsigned int ci=0; ci<cells[i].CCF.size(); ci++)
	  {
	    if(cells[i].CCF[ci]==1 && aliveFlagList[id2indexDic[cells[i].IOCC[ci]]]==1)
	    {
	      countDic_[id2indexDic[cells[i].IOCC[ci]]] = countDic[i];
	    }
	  }
	}
      }

      preConnectedCount = connectedCount;
      connectedCount = 0;
      for (const auto& entry : countDic_)
      {
	int i  = entry.first;
	int c  = entry.second;
	countDic[i] = c;
	if(0<c)
	{
	  connectedCount++;
	}
      }

      if(connectedCount==preConnectedCount)
      {
	break;
      }
      
    }

    for (const auto& entry : countDic)
    {
      int i  = entry.first;
      int c  = entry.second;
      if(0<c)
      {
	addSelectedCell(i);
      }
    }

    lastSelectedCellIndex = -1;
    
  }

  void write(std::string filename)
  {

    int newId, numCellsToExport;
    int count = 0;
    std::map<int, std::map<int, int>> groupId2Id;
    std::map<int, std::map<int, int>> groupId2alive;

    newId = 1;
    for(long unsigned int index=0; index<cells.size(); index++)
    {
      if(aliveFlagList[index]==1)
      {
	groupId2Id[groupList[index]][cells[index].ID] = newId;
	groupId2alive[groupList[index]][cells[index].ID] = 1;
	newId++;
      }
      else
      {
	groupId2alive[groupList[index]][cells[index].ID] = 0;
      }
    }

    numCellsToExport = newId - 1;

    std::ofstream newCellsFile(filename, std::ios::out);

    newCellsFile << numCellsToExport << "\n";
    
    for(long unsigned int index=0; index<cells.size(); index++)
    {

      if(aliveFlagList[index]==0)
      {
	continue;
      }

      count++; 
      std::cout << index << " : " << count << " / " << numCellsToExport << std::endl;
      
      Cell cell = cells[index];
      
      for(int li=0; li<oneCellLen; li++)
      {
        
        if(li==0)
	{
	  //exist
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==1)
        {
	  //book
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==2)
        {
	  //bookmarker
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==3)
        {
	  //bookmarker_advance
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==4)
        {
	  //xyz
	  std::stringstream ss;
	  ss << cell.x << " ";
	  ss << cell.y << " ";
	  ss << cell.z << " \n";
	  newCellsFile << ss.str();
        }
        else if(li==5)
        {
	  //nxnynz
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==6)
        {
	  //vxvyvz
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==7)
        {
	  //fxfyfz
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==8)
        {
	  //crcgcb
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==9)
        {
	  //aragab
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==10)
        {
	  //IOLr
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==11)
        {
	  //IOLg
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==12)
        {
	  //IOLb
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==13)
        {
	  //m
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==14)
        {
	  //r
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==15)
        {
	  //E
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==16)
        {
	  //alpha
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==17)
        {
	  //ID	  
	  std::stringstream ss;
	  ss << groupId2Id[groupList[index]][cells[index].ID];
	  ss << "\n";
	  newCellsFile << ss.str();
        }
        else if(li==18)
        {
	  //age
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==19)
        {
	  //in_data
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==20)
        {
	  //out_data
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==21)
        {
	  //w1
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==22)
        {
	  //w2
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==23)
        {
	  //KOCC
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==24)
        {
	  //LOCC
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==25)
        {
	  //SOCC
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==26)
        {
	  //CCF
	  std::stringstream ss;
	  for (const auto& v : cell.IOCC)
	  {
	    if(v==0)
	    {
	      ss << "0 ";
	    }
	    else
	    {
	      if(groupId2alive[groupList[index]][v]==1)
	      {
		ss << "1 ";
	      }
	      else
	      {
		ss << "0 ";
	      }
	    }
	  }
	  
	  ss << "\n";
	  newCellsFile << ss.str();
        }
        else if(li==27)
        {
	  //IOCC
	  std::stringstream ss;
	  for (const auto& v : cell.IOCC)
	  {
	    if(v==0)
	    {
	      ss << "0 ";
	    }
	    else
	    {
	      if(groupId2alive[groupList[index]][v]==1)
	      {
		ss << groupId2Id[groupList[index]][v] << " ";
	      }
	      else
	      {
		ss << "0 ";
	      }
	    }
	  }
	  
	  ss << "\n";
	  newCellsFile << ss.str();
        }
        else if(li==28)
        {
	  //UIOCC
	  int i=0;
	  std::stringstream ss;
	  for (const auto& v : cell.IOCC)
	  {
	    if(v==0)
	    {
	      ss << "0 ";
	    }
	    else
	    {
	      if(groupId2alive[groupList[index]][v]==1)
	      {
		//ss << groupId2Id[groupList[index]][v] << " ";
		ss << cell.UIOCC[i] << " ";
	      }
	      else
	      {
		ss << "0 ";
	      }
	    }
	    i++;
	  }
	  
	  ss << "\n";
	  newCellsFile << ss.str();
	  
	  //newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==29)
        {
	  //HIT_BLOCK_AF
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==30)
        {
	  //HIT_CELL_AF
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==31)
        {
	  //SPRING_AF
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==32)
        {
	  //MECHANICS_AF
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==33)
        {
	  //EAT_AF
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==34)
        {
	  //FUSION_AF
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==35)
        {
	  //LIGHT_AF
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==36)
        {
	  //INFO_TRANS_F
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==37)
        {
	  //NEURAL_NETWORK_F
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==38)
        {
	  //WAIT_FOR_CONNECT_F
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==39)
        {
	  //WAIT_FOR_CONNECT_UI
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==40)
        {
	  //WAIT_FOR_DISCONNECT_F
	  newCellsFile << cell.rawData[li] << "\n";
        }
        else if(li==41)
        {
	  //ALONE_F
	  newCellsFile << cell.rawData[li] << "\n";
        }
  
      }

    }

    newCellsFile.close();
    
  }
  
};



//全体の流れを参考にしたサイト
//https://sonson.jp/blog/2006/04/03/opengl-bitmap-1/

//最終的に一番参考になったサイト
//https://www.mm2d.net/main/prog/c/image_io-05.html
//このページだけでなく、前後のBMP関連のページは全て参考になる。

//BMPのフォーマとについてまとまっている。
//http://www.umekkii.jp/data/computer/file_format/bitmap.cgi

//その他
//https://www.fit.ac.jp/elec/7_online/lu/sample/bmp_image_proc.cpp
//https://npal-shared.hatenablog.com/entry/20121107/1352284053
typedef struct _BitmapHeader{
  uint16_t bfType;      /**< ファイルタイプ、必ず"BM" */
  uint32_t bfSize;      /**< ファイルサイズ */
  uint16_t bfReserved1; /**< リザーブ */
  uint16_t bfReserved2; /**< リサーブ */
  uint32_t bfOffBits;   /**< 先頭から画像情報までのオフセット */
} BitmapHeader;

typedef struct _BitmapInfoHeader {
  uint32_t biSize;          /**< この構造体のサイズ */
  int32_t  biWidth;         /**< 画像の幅 */
  int32_t  biHeight;        /**< 画像の高さ */
  uint16_t biPlanes;        /**< 画像の枚数、通常1 */
  uint16_t biBitCount;      /**< 一色のビット数 */
  uint32_t biCompression;   /**< 圧縮形式 */
  uint32_t biSizeImage;     /**< 画像領域のサイズ */
  int32_t  biXPelsPerMeter; /**< 画像の横方向解像度情報 */
  int32_t  biYPelsPerMeter; /**< 画像の縦方向解像度情報*/
  uint32_t biClrUsed;       /**< カラーパレットのうち実際に使っている色の個数 */
  uint32_t biClrImportant;  /**< カラーパレットのうち重要な色の数 */
} BitmapInfoHeader;

void InitHeaders(BitmapHeader* header, BitmapInfoHeader* info){
  header->bfType        = FILE_TYPE; /**< ファイルタイプ、必ず"BM" */
  header->bfSize        =  0       ; /**< ファイルサイズ */
  header->bfReserved1   =  0       ; /**< リザーブ */
  header->bfReserved2   =  0       ; /**< リザーブ */
  header->bfOffBits     =  0       ; /**< 先頭から画像情報までのオフセット */
			           
  info->biSize          =  INFO_HEADER_SIZE; /**< この構造体のサイズ */
  info->biWidth         =  0       ; /**< 画像の幅 */
  info->biHeight        =  0       ; /**< 画像の高さ */
  info->biPlanes        =  1       ; /**< 画像の枚数、通常1 */
  info->biBitCount      =  24      ; /**< 一色のビット数 */
  info->biCompression   =  0       ; /**< 圧縮形式 */
  info->biSizeImage     =  0       ; /**< 画像領域のサイズ */
  info->biXPelsPerMeter =  0       ; /**< 画像の横方向解像度情報 */
  info->biYPelsPerMeter =  0       ; /**< 画像の縦方向解像度情報*/
  info->biClrUsed       =  0       ; /**< カラーパレットのうち実際に使っている色の個数 */
  info->biClrImportant  =  0       ; /**< カラーパレットのうち重要な色の数 */
}

int WriteBitmap(const char* filename, GLubyte* data, int width, int height){
  FILE *fp;
  BitmapHeader header;
  BitmapInfoHeader info;
  int x;
  int y;
  
  // ファイルオープン
  if( ( fp = fopen(filename, "wb") )==NULL){
    return -1;
  }

  //ヘッダ構造体の初期化
  InitHeaders(&header, &info);
  //Bitmapサイズ
  info.biWidth  = (int32_t)width;
  info.biHeight = (int32_t)height;
  int writeWidth;
  
  // データの幅のバイト数が4の倍数であるかをチェック
  if( width*3%4 == 0)
  {
    writeWidth = width*3;
  }
  else
  {
    // そうでなければ，4の倍数にあわせた幅のバイトサイズにする
    writeWidth = width*3 + 4 - (width*3)%4;
  }
  
  //ファイル容量
  header.bfSize = (uint32_t)(FILE_HEADER_SIZE + info.biSize + writeWidth*height); //ビット情報量
  header.bfOffBits = (uint32_t)(FILE_HEADER_SIZE + info.biSize); 
  info.biSizeImage = (uint32_t)(writeWidth*height); 

  //write header
  fwrite(&header.bfType,      sizeof(header.bfType),      1, fp);
  fwrite(&header.bfSize,      sizeof(header.bfSize),      1, fp);
  fwrite(&header.bfReserved1, sizeof(header.bfReserved1), 1, fp);
  fwrite(&header.bfReserved2, sizeof(header.bfReserved2), 1, fp);
  fwrite(&header.bfOffBits,   sizeof(header.bfOffBits),   1, fp);

  //write info
  fwrite(&info.biSize,          sizeof(info.biSize),          1, fp);
  fwrite(&info.biWidth,         sizeof(info.biWidth),         1, fp);
  fwrite(&info.biHeight,        sizeof(info.biHeight),        1, fp);
  fwrite(&info.biPlanes,        sizeof(info.biPlanes),        1, fp);
  fwrite(&info.biBitCount,      sizeof(info.biBitCount),      1, fp);
  fwrite(&info.biCompression,   sizeof(info.biCompression),   1, fp);
  fwrite(&info.biSizeImage,     sizeof(info.biSizeImage),     1, fp);
  fwrite(&info.biXPelsPerMeter, sizeof(info.biXPelsPerMeter), 1, fp);
  fwrite(&info.biYPelsPerMeter, sizeof(info.biYPelsPerMeter), 1, fp);
  fwrite(&info.biClrUsed,       sizeof(info.biClrUsed),       1, fp);
  fwrite(&info.biClrImportant,  sizeof(info.biClrImportant),  1, fp);
  
  unsigned char zero=0;
  // イメージデータ書き込み
  for( y=0 ; y < height ; y++ )
  {
    // データをBGRの順で書き込み
    for( x=0 ; x < width ; x++ )
    {
      //int j;
      fwrite((data+x*3+3*y*width+2),sizeof(GLubyte),1,fp);
      fwrite((data+x*3+3*y*width+1),sizeof(GLubyte),1,fp);
      fwrite((data+x*3+3*y*width),sizeof(GLubyte),1,fp);
    }
    // 幅のバイト数が4の倍数でないときは０で埋める
    if( width*3%4 != 0)
    {
      for( int j=0;j<4-(width*3)%4;j++)
      {
	fwrite(&zero, sizeof(GLubyte), 1, fp);
      }
    }
  }
  // ファイルクローズ
  fclose(fp);
  return 0;
}


class App{

public:

  double PI = 3.14159265358979323846264338;
  
  //grep
  static int WIN_WIDTH;                 // ウィンドウの幅
  static int WIN_HEIGHT;                 // ウィンドウの高さ
  static int WIN_WIDTH_PIXEL;                 // ウィンドウの幅
  static int WIN_HEIGHT_PIXEL;                 // ウィンドウの高さ
  static constexpr const char *WIN_TITLE = "Praparat";     // ウィンドウのタイトル
  static constexpr const double fps = 60.0;

  static int MOTION_LEFT_BUTTON_FLAG;
  static int MOTION_RIGHT_BUTTON_FLAG;
  static int SHOW_IT_FLAG;
  static int SHOW_FIELD_MODE;
  static int SHOW_CELL_MODE;
  int REC_FLAG = 0;

  static int SCROLL_ACTIVE;
  static int KEY_ACTIVE;
  static int SELECTION_MODE;
  static int CURRENT_SELECTED_CELL_INDEX;
  bool CURSOR_ON_ImGuiWINDOW;

  static double LOOKAT_EX;
  static double LOOKAT_EY;
  static double LOOKAT_EZ;
  static double LOOKAT_CX;
  static double LOOKAT_CY;
  static double LOOKAT_CZ;
  
  static double SHOW_ENERGY_COEFF;
  
  double MAX_ENERGY = 100.0;
  double MAX_AGE    = 100.0;

  static int CAMERA_MODE;
  static const int SELECTION_MODE_MAX = 4;
  static const int SHOW_CELL_MODE_MAX = 9;
  static const int SHOW_FIELD_MODE_MAX = 2;
  
  double SPHERE_POLE[2][3] = {0};
  double SPHERE_POINT[SPHERE_DIV_NUM-1][2*SPHERE_DIV_NUM+1][3] = {0};

  static float TRANSLATE_X;
  static float TRANSLATE_Y;
  static float TRANSLATE_Z;
  
  CellManager AppCM;
  FieldManager AppFM;
  
  //int FSx;
  //int FSy;
  //int FSz;
  //
  //int FCx;
  //int FCy;
  //int FCz;
  
  int NumberOfCell;
  
  GLdouble VERTEX[8][3] = {
			   { 0.0, 0.0, 0.0 },
			   { 1.0, 0.0, 0.0 },
			   { 1.0, 1.0, 0.0 },
			   { 0.0, 1.0, 0.0 },
			   { 0.0, 0.0, 1.0 },
			   { 1.0, 0.0, 1.0 },
			   { 1.0, 1.0, 1.0 },
			   { 0.0, 1.0, 1.0 }};

  int EDGE[12][2] = {
		     { 0, 1 },
		     { 1, 2 },
		     { 2, 3 },
		     { 3, 0 },
		     { 4, 5 },
		     { 5, 6 },
		     { 6, 7 },
		     { 7, 4 },
		     { 0, 4 },
		     { 1, 5 },
		     { 2, 6 },
		     { 3, 7 }};
  

  static constexpr const double MOTION_RATE = 50.0;

  static int BMP_COUNT;
  
  static void windowSizeCB(GLFWwindow *window, int w, int h) {
    //printf("windowSizeCB %d %d\n", w, h);
    
    WIN_WIDTH = w;
    WIN_HEIGHT = h;
    
    WIN_WIDTH_PIXEL  = w;
    WIN_HEIGHT_PIXEL = h;
    
    glViewport(0, 0, w, h);
    
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(30.0, (double)w / (double)h, 1.0, 500.0);
    
    glMatrixMode(GL_MODELVIEW);
    
  }

  void resize(GLFWwindow *window, int w, int h)
  {

    int renderBufferWidth, renderBufferHeight;
    glfwGetFramebufferSize(window, &renderBufferWidth, &renderBufferHeight);
    
    WIN_WIDTH_PIXEL  = renderBufferWidth;
    WIN_HEIGHT_PIXEL = renderBufferHeight;

    glViewport(0, 0, w, h);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(30.0, (double)w / (double)h, 1.0, 500.0);
    
    glMatrixMode(GL_MODELVIEW);
  }
  
  void resize4apple(GLFWwindow *window, int width, int height)
  {

    glfwSetWindowSize(window, WIN_WIDTH, WIN_HEIGHT);
    
    int renderBufferWidth, renderBufferHeight;
    glfwGetFramebufferSize(window, &renderBufferWidth, &renderBufferHeight);
    
    WIN_WIDTH_PIXEL  = renderBufferWidth;
    WIN_HEIGHT_PIXEL = renderBufferHeight;
  
    glViewport(0, 0, renderBufferWidth, renderBufferHeight);

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(30.0, (double)width / (double)height, 1.0, 500.0);
    
    glMatrixMode(GL_MODELVIEW);
    
  }
  
  static void mouseButtonCB(GLFWwindow *window, int button, int action, int mods) {
    //printf("mouseButtonCB %d %d %d\n", button, action, mods);
    
    if (action==1)
    {
      if (button==0)
      {
	MOTION_LEFT_BUTTON_FLAG = 1;
      }
      else
      {
	MOTION_RIGHT_BUTTON_FLAG = 1;
      }
    }
    else
    {
      if (button==0)
      {
	MOTION_LEFT_BUTTON_FLAG = 0;
      }
      else
      {
	MOTION_RIGHT_BUTTON_FLAG = 0;
      }
    }  
  }

  static void mousePosCB(GLFWwindow *window, double x, double y) {
    //printf("mousePosCB %.1lf %.1lf, %d, %d\n", x, y, MOTION_LEFT_BUTTON_FLAG, SHOW_IT_FLAG);
  
    static int flag = 0;
    static int preX, preY;
    double dx, dy, dz, rad, phi, theta;
    double r = sqrt((LOOKAT_CX-LOOKAT_EX)*(LOOKAT_CX-LOOKAT_EX)
		    +(LOOKAT_CY-LOOKAT_EY)*(LOOKAT_CY-LOOKAT_EY)
		    +(LOOKAT_CZ-LOOKAT_EZ)*(LOOKAT_CZ-LOOKAT_EZ));
    
    if(MOTION_RIGHT_BUTTON_FLAG==0)
    {
      flag = 0;
    }
    else
    {
      if (flag==0)
      {
	preX = x;
	preY = y;
	flag = 1;
      }
      else
      {
	r = MOTION_RATE*r;
	dx = LOOKAT_CX - LOOKAT_EX;
	dy = LOOKAT_CY - LOOKAT_EY;
	dz = LOOKAT_CZ - LOOKAT_EZ;
	phi = acos(dx/sqrt(dx*dx + dz*dz));
	if(dz>0.0)
	{
	  phi=-phi;
	}
	rotation_y(&dx, &dy, &dz, -phi);
	theta = acos(dx/sqrt(dx*dx + dy*dy));
	rotation_z(&dx, &dy, &dz, -theta);
	
	rad = (double)(preY - y)/r;
	rotation_z(&dx, &dy, &dz, theta + rad);
	
	rad = (double)(preX - x)/r;
	rotation_y(&dx, &dy, &dz, phi + rad);
	
	LOOKAT_CX = dx + LOOKAT_EX;
	LOOKAT_CY = dy + LOOKAT_EY;
	LOOKAT_CZ = dz + LOOKAT_EZ;
	
	preX = x;
	preY = y;
      } 
    }  
  }

  static void mouseScrollCB(GLFWwindow *window, double x, double y) {

    if(SCROLL_ACTIVE==1)
    {
      if(0.0<y)
      {
	if(SELECTION_MODE<SELECTION_MODE_MAX)
	{
	  SELECTION_MODE += 1;
	}
	else
	{
	  SELECTION_MODE = 0;
	}
      }
      else
      {
	if(0<SELECTION_MODE)
	{
	  SELECTION_MODE -= 1;
	}
	else
	{
	  SELECTION_MODE = SELECTION_MODE_MAX;
	}
      }
    }

  }

  static void keyFunCB(GLFWwindow* window, int key, int scancode, int action, int mods) {
    //printf("keyFunCB %d %d %d %d\n", key, scancode, action, mods);

    int width = WIN_WIDTH_PIXEL;
    int height = WIN_HEIGHT_PIXEL;
    double dx, dy, dz, r, l;
    double cx, cy, cz;
    GLubyte* pixel_data = (GLubyte*)malloc((width)*(height)*3*(sizeof(GLubyte)));
    char filename[256];
    
    int LOOKAT_SMOOTH_FLAG = 0;
    
    double LOOKAT_MOVE_RATE = 100.0;
    double LOOKAT_MOVE_STEP = 0.4;
    
    double LOOKAT_DT = 0.01;
    double LOOKAT_VX = 0.0;
    double LOOKAT_VY = 0.0;
    double LOOKAT_VZ = 0.0;


    if(KEY_ACTIVE==0)
    {
      return;
    }

    
    switch (key){
      
    case 'a':
    case 'A':
      if(LOOKAT_SMOOTH_FLAG==1)
      {
	dx = LOOKAT_CX - LOOKAT_EX;
	dz = LOOKAT_CZ - LOOKAT_EZ;
	r = sqrt(dx*dx + dz*dz);
	LOOKAT_VX += LOOKAT_MOVE_RATE*(dz/r)*LOOKAT_DT;
	LOOKAT_VZ += -LOOKAT_MOVE_RATE*(dx/r)*LOOKAT_DT;
      }
      else
      {
	dx = LOOKAT_CX - LOOKAT_EX;
	dz = LOOKAT_CZ - LOOKAT_EZ;
	r = sqrt(dx*dx + dz*dz);
	LOOKAT_EX += (LOOKAT_MOVE_STEP*dz)/r;
	LOOKAT_CX += (LOOKAT_MOVE_STEP*dz)/r;
	LOOKAT_EZ += -(LOOKAT_MOVE_STEP*dx)/r;
	LOOKAT_CZ += -(LOOKAT_MOVE_STEP*dx)/r;
      }  
      break;

    case 'b':
    case 'B':
      break;
      
    case 'n':
    case 'N':
      break;
    
    case 'w':
    case 'W':
      if(LOOKAT_SMOOTH_FLAG==1)
      {
	dx = LOOKAT_CX - LOOKAT_EX;
	dz = LOOKAT_CZ - LOOKAT_EZ;
	r = sqrt(dx*dx + dz*dz);
	LOOKAT_VX += LOOKAT_MOVE_RATE*(dx/r)*LOOKAT_DT;
      }
      else
      {
	dx = LOOKAT_CX - LOOKAT_EX;
	dz = LOOKAT_CZ - LOOKAT_EZ;
	r = sqrt(dx*dx + dz*dz);
	LOOKAT_EX += (LOOKAT_MOVE_STEP*dx)/r;
	LOOKAT_CX += (LOOKAT_MOVE_STEP*dx)/r;
	LOOKAT_EZ += (LOOKAT_MOVE_STEP*dz)/r;
	LOOKAT_CZ += (LOOKAT_MOVE_STEP*dz)/r;
      }
      break;
      
    case 'd':
    case 'D':
      if(LOOKAT_SMOOTH_FLAG==1)
      {
	dx = LOOKAT_CX - LOOKAT_EX;
	dz = LOOKAT_CZ - LOOKAT_EZ;
	r = sqrt(dx*dx + dz*dz);
	LOOKAT_VX += -LOOKAT_MOVE_RATE*(dz/r)*LOOKAT_DT;
	LOOKAT_VZ += LOOKAT_MOVE_RATE*(dx/r)*LOOKAT_DT;
      }
      else
      {
	dx = LOOKAT_CX - LOOKAT_EX;
	dz = LOOKAT_CZ - LOOKAT_EZ;
	r = sqrt(dx*dx + dz*dz);
	LOOKAT_EX += -(LOOKAT_MOVE_STEP*dz)/r;
	LOOKAT_CX += -(LOOKAT_MOVE_STEP*dz)/r;
	LOOKAT_EZ += (LOOKAT_MOVE_STEP*dx)/r;
	LOOKAT_CZ += (LOOKAT_MOVE_STEP*dx)/r;
      }
      break;
      
    case 's':
    case 'S':
      if(LOOKAT_SMOOTH_FLAG==1)
      {
	dx = LOOKAT_CX - LOOKAT_EX;
	dz = LOOKAT_CZ - LOOKAT_EZ;
	r = sqrt(dx*dx + dz*dz);
	LOOKAT_VX += -LOOKAT_MOVE_RATE*(dx/r)*LOOKAT_DT;
	LOOKAT_VZ += -LOOKAT_MOVE_RATE*(dz/r)*LOOKAT_DT;
      }
      else
      {
	dx = LOOKAT_CX - LOOKAT_EX;
	dz = LOOKAT_CZ - LOOKAT_EZ;
	r = sqrt(dx*dx + dz*dz);
	LOOKAT_EX += -(LOOKAT_MOVE_STEP*dx)/r;
	LOOKAT_CX += -(LOOKAT_MOVE_STEP*dx)/r;
	LOOKAT_EZ += -(LOOKAT_MOVE_STEP*dz)/r;
	LOOKAT_CZ += -(LOOKAT_MOVE_STEP*dz)/r;
      }
      break;

    case 'k':
    case 'K':
      if(action==1)
      {
	if(CAMERA_MODE<3)
	{
	  CAMERA_MODE = CAMERA_MODE + 1;
	}
	else
	{
	  CAMERA_MODE = 0;
	}


	dx = LOOKAT_CX - LOOKAT_EX;
	dy = LOOKAT_CY - LOOKAT_EY;
	dz = LOOKAT_CZ - LOOKAT_EZ;
	r = sqrt(dx*dx + dy*dy + dz*dz);
	
	switch (CAMERA_MODE) {
	case 0:  
	  printf("CHANGE VIEW MODE 0\n");
	  LOOKAT_EX = -5.0;
	  LOOKAT_EY = 2.0;
	  LOOKAT_EZ = 0.0;
	  cx = 0.0;
	  cy = 0.0;
	  cz = 0.0;  
	  break;
	
	case 1:
	  printf("CHANGE VIEW MODE 1\n");
	  LOOKAT_EX = 40.0;
	  LOOKAT_EY = 15.0;
	  LOOKAT_EZ = 0.0;
	  cx = 0.0;
	  cy = 0.0;
	  cz = 0.0;
	  break;
	  
	case 2:
	  printf("CHANGE VIEW MODE 2\n");
	  LOOKAT_EX = 0.0;
	  LOOKAT_EY = 10.0;
	  LOOKAT_EZ = 0.0;
	  cx = 0.1;
	  cy = -10.0;
	  cz = 0.0;
	  break;
	
	case 3:
	  printf("CHANGE VIEW MODE 3\n");
	  LOOKAT_EX = 0.0;
	  LOOKAT_EY = 15.0;
	  LOOKAT_EZ = 40.0;
	  cx = 0.0;
	  cy = 0.0;
	  cz = 0.0;
	  break;
	}

	dx = cx - LOOKAT_EX;
	dy = cy - LOOKAT_EY;
	dz = cz - LOOKAT_EZ;
	l = sqrt(dx*dx + dy*dy + dz*dz);
	  
	LOOKAT_CX = LOOKAT_EX + r*(dx/l);
	LOOKAT_CY = LOOKAT_EY + r*(dy/l);
	LOOKAT_CZ = LOOKAT_EZ + r*(dz/l);


      }
      break;

    case ' ':
      if(LOOKAT_SMOOTH_FLAG==1)
      {
	LOOKAT_VY += LOOKAT_MOVE_RATE*LOOKAT_DT;
      }
      else
      {
	LOOKAT_EY += LOOKAT_MOVE_STEP;
	LOOKAT_CY += LOOKAT_MOVE_STEP;
      }
      break;
      
    case 'f':
    case 'F':
      if(LOOKAT_SMOOTH_FLAG==1)
      {
	LOOKAT_VY += -LOOKAT_MOVE_RATE*LOOKAT_DT;
      }
      else
      {
	LOOKAT_EY += -LOOKAT_MOVE_STEP;
	LOOKAT_CY += -LOOKAT_MOVE_STEP;
      }
      break;
      
    case 'm':
    case 'M':
      break;
      
    case 'l':
    case 'L':
      if(action==1)
      {
	printf("LOOKAT_EX, LOOKAT_EY, LOOKAT_EZ\n");
	printf("  %lf, %lf, %lf\n", LOOKAT_EX, LOOKAT_EY, LOOKAT_EZ);
	printf("LOOKAT_CX, LOOKAT_CY, LOOKAT_CZ\n");
	printf("  %lf, %lf, %lf\n", LOOKAT_CX, LOOKAT_CY, LOOKAT_CZ);
      }
      break;
      
    case 'p':
    case 'P':
      break;
      
    case 'i':
    case 'I':
      break;
      
    case 'h':
    case 'H':
      if(action==1)
      {
	SHOW_IT_FLAG = 1 - SHOW_IT_FLAG; 
      }
      break;
      
    case 'o':
    case 'O':
      break;
      
    case 'c':
    case 'C':
      if(action==1)
      {
	if(SHOW_CELL_MODE<SHOW_CELL_MODE_MAX)
	{
	  SHOW_CELL_MODE = SHOW_CELL_MODE + 1;
	}
	else
	{
	  SHOW_CELL_MODE = 0;
	}
      }
      break;
      
    case 'r':
    case 'R':
      if(action==1)
      {
	if(SHOW_FIELD_MODE<SHOW_FIELD_MODE_MAX)
	{
	  SHOW_FIELD_MODE = SHOW_FIELD_MODE + 1;
	}
	else
	{
	  SHOW_FIELD_MODE = 0;
	}
      }
      break;
      
    case 't':
    case 'T':
      break;
      
    case 'u':
    case 'U':
      break;
      
    case 'y':
    case 'Y':
      if(action==1)
      {
	glReadBuffer( GL_BACK ); //GL_FRONT:フロントバッファ　GL_BACK:バックバッファ
	glReadPixels(
		     0, 0,
		     width, height,
		     GL_RGB,
		     GL_UNSIGNED_BYTE,
		     pixel_data);
	sprintf(filename, "./output%08d.bmp", BMP_COUNT);
	WriteBitmap(filename, pixel_data, width, height);
	free(pixel_data);
	BMP_COUNT++;
      }
      break;			

    case 'v':
    case 'V':
      break;
      
    case ',':
      break;
      
    case '.':
      break;

    case 265:
      if(action==1)
      {
	if(SELECTION_MODE<SELECTION_MODE_MAX)
	{
	  SELECTION_MODE += 1;
	}
	else
	{
	  SELECTION_MODE = 0;
	}
      }
      break;
      
    case 264:
      if(action==1)
      {
	if(0<SELECTION_MODE)
	{
	  SELECTION_MODE -= 1;
	}
	else
	{
	  SELECTION_MODE = SELECTION_MODE_MAX;
	}
      }
      break;
      
      
    case 'q':
    case 'Q':
    case '\033':  /* '\033' は ESC の ASCII コード */
      exit(0);
      
    default:
      break;
    }
  }

  static void charFunCB(GLFWwindow* window, unsigned int charInfo) {
    //printf("charFunCB %d\n", charInfo);
  }

  static void dropCB(GLFWwindow *window, int num, const char **paths) {
    //printf("dropCB %d\n", num);
    for (int i = 0; i < num; i++)
    {
      printf("%s\n", paths[i]);
    }
  }

  //void *pstep(void *arg)
  //{
  //  int n = *(int*)arg;
  //  int m = 1;
  //
  //  while(1)
  //  {
  //    if (MOVE_FLAG==1)
  //    {
  //      step(&m);
  //    }
  //  }
  //  return NULL;
  //}
  
  
  void drawAxis()
  {

    double vec[3];
    glColor3d(1.0, 0.0, 0.0);
    glBegin(GL_LINES);
    vec[0] = 0.0;
    vec[1] = 0.0;
    vec[2] = 0.0;
    glVertex3dv(vec);
    vec[0] = 1.0;
    vec[1] = 0.0;
    vec[2] = 0.0;
    glVertex3dv(vec);
    glEnd();
    
    glColor3d(0.0, 1.0, 0.0);
    glBegin(GL_LINES);
    vec[0] = 0.0;
    vec[1] = 0.0;
    vec[2] = 0.0;
    glVertex3dv(vec);
    vec[0] = 0.0;
    vec[1] = 1.0;
    vec[2] = 0.0;
    glVertex3dv(vec);
    glEnd();

    glColor3d(0.0, 0.0, 1.0);
    glBegin(GL_LINES);
    vec[0] = 0.0;
    vec[1] = 0.0;
    vec[2] = 0.0;
    glVertex3dv(vec);
    vec[0] = 0.0;
    vec[1] = 0.0;
    vec[2] = 1.0;
    glVertex3dv(vec);
    glEnd();
  }

  void drawWireCube(double x, double y, double z, double lx, double ly, double lz, double r, double g, double b)
  {
    int i;
    double p1[3], p2[3];
    
    glColor3d(r, g, b);
    glBegin(GL_LINES);
    for (i = 0; i < 12; ++i)
    {
      p1[0] = x + lx*(VERTEX[EDGE[i][0]][0] - 0.5);
      p1[1] = y + ly*(VERTEX[EDGE[i][0]][1] - 0.5);
      p1[2] = z + lz*(VERTEX[EDGE[i][0]][2] - 0.5);
      
      p2[0] = x + lx*(VERTEX[EDGE[i][1]][0] - 0.5);
      p2[1] = y + ly*(VERTEX[EDGE[i][1]][1] - 0.5);
      p2[2] = z + lz*(VERTEX[EDGE[i][1]][2] - 0.5);
      
      glVertex3dv(p1);
      glVertex3dv(p2);
    }
    glEnd();
  }
  
  void drawWireSphere(double x, double y, double z, double radius, int n, int m, double r, double g, double b)
  {
    int i, j, mm;
    double dphi, dthe;
    double phi, the;
    double p1[3], p2[3];
    double tp1[3], tp2[3];
    
    dphi = PI/(double)n;
    dthe = PI/(double)m;
    phi = 0.0;
    the = 0.0;
    mm = 2*m;
    
    glColor3d(r, g, b);
    
    for(j=0;j<=mm;j++)
    {
      p1[0] = 0.0;
      p1[1] = radius;
      p1[2] = 0.0;
      
      glBegin(GL_LINES);
      for(i=0;i<=n;i++)
      {
	p2[0] = 0.0;
	p2[1] = radius;
	p2[2] = 0.0;
	rotation_x(&p2[0], &p2[1], &p2[2], (double)i*dthe);
	rotation_y(&p2[0], &p2[1], &p2[2], phi);
	tp1[0] = x + p1[0];
	tp1[1] = y + p1[1];
	tp1[2] = z + p1[2];
	tp2[0] = x + p2[0];
	tp2[1] = y + p2[1];
	tp2[2] = z + p2[2];
	glVertex3dv(tp1);
	glVertex3dv(tp2);
	p1[0] = p2[0];
	p1[1] = p2[1];
	p1[2] = p2[2];
      }
      glEnd();
      
      phi += dphi;
    }

    for(i=0;i<=n;i++)
    {
      p1[0] = 0.0;
      p1[1] = radius;
      p1[2] = 0.0;
      
      rotation_x(&p1[0], &p1[1], &p1[2], the);
      
      p2[0] = p1[0];
      p2[1] = p1[1];
      p2[2] = p1[2];
      
      glBegin(GL_LINES);
      for(j=0;j<=mm;j++)
      {
	  
	rotation_y(&p2[0], &p2[1], &p2[2], dphi);
	tp1[0] = x + p1[0];
	tp1[1] = y + p1[1];
	tp1[2] = z + p1[2];
	tp2[0] = x + p2[0];
	tp2[1] = y + p2[1];
	tp2[2] = z + p2[2];
	glVertex3dv(tp1);
	glVertex3dv(tp2);
	p1[0] = p2[0];
	p1[1] = p2[1];
	p1[2] = p2[2];
	
      }
      glEnd();
      
      the += dthe;
    }
  }

  void drawSolidSphere(double x, double y, double z, double radius, int n, int m, double r, double g, double b)
  {
    int i, j, mm;
    double dphi, dthe;
    double phi, the;
    double p1[3], p2[3], p3[3], p4[3];
    double tp1[3], tp2[3], tp3[3], tp4[3];
    
    if(n%2==1)
    {
      n++;
    }
    
    if(m%2==1)
    {
      m++;
    }

    dphi = PI/(double)n;
    dthe = PI/(double)m;
    phi = 0.0;
    the = 0.0;
    mm = 2*m;
    
    glColor3d(r, g, b);
    
    for(i=0;i<n;i++)
    {
      for(j=0;j<mm;j++)
      {
	glBegin(GL_TRIANGLE_FAN);
	if(j==0)
	{
	  phi = 0.0;
	  
	  p1[0] = 0.0;
	  p1[1] = radius;
	  p1[2] = 0.0;
	  
	  p2[0] = 0.0;
	  p2[1] = radius;
	  p2[2] = 0.0;
	  
	  p3[0] = 0.0;
	  p3[1] = radius;
	  p3[2] = 0.0;
	  
	  p4[0] = 0.0;
	  p4[1] = radius;
	  p4[2] = 0.0;
	
	  rotation_x(&p1[0], &p1[1], &p1[2], the);
	  rotation_y(&p1[0], &p1[1], &p1[2], phi);
	  
	  rotation_x(&p2[0], &p2[1], &p2[2], the + dthe);
	  rotation_y(&p2[0], &p2[1], &p2[2], phi);
	  
	  rotation_x(&p3[0], &p3[1], &p3[2], the + dthe);
	  rotation_y(&p3[0], &p3[1], &p3[2], phi + dphi);
	  
	  rotation_x(&p4[0], &p4[1], &p4[2], the);
	  rotation_y(&p4[0], &p4[1], &p4[2], phi + dphi);
	  
	}
	else
	{
	  rotation_y(&p1[0], &p1[1], &p1[2], dphi);
	  rotation_y(&p2[0], &p2[1], &p2[2], dphi);
	  rotation_y(&p3[0], &p3[1], &p3[2], dphi);
	  rotation_y(&p4[0], &p4[1], &p4[2], dphi);	
	}
      
	tp1[0] = x + p1[0];
	tp1[1] = y + p1[1];
	tp1[2] = z + p1[2];
	
	tp2[0] = x + p2[0];
	tp2[1] = y + p2[1];
	tp2[2] = z + p2[2];
	
	tp3[0] = x + p3[0];
	tp3[1] = y + p3[1];
	tp3[2] = z + p3[2];
	
	tp4[0] = x + p4[0];
	tp4[1] = y + p4[1];
	tp4[2] = z + p4[2];
	
	if(i==0)
	{
	  glVertex3dv(tp1);
	  glVertex3dv(tp2);
	  glVertex3dv(tp3);
	}
	else if(i==(n-1))
	{
	  glVertex3dv(tp1);
	  glVertex3dv(tp2);
	  glVertex3dv(tp4);
	}
	else
	{
	  glVertex3dv(tp1);
	  glVertex3dv(tp2);
	  glVertex3dv(tp3);
	  glVertex3dv(tp4);
	}
	
	glEnd();
      }
      
      the += dthe;
    }
  }

  void setSphere()
  {
    int i, j, n, nn, row, col;
    double dphi, dthe;
    double phi, the;
    double p[3];
    
    n = SPHERE_DIV_NUM;
    
    dphi = PI/(double)n;
    dthe = PI/(double)n;
    phi = 0.0;
    the = 0.0;
    nn = 2*n;
    
    SPHERE_POLE[0][0] = 0.0;
    SPHERE_POLE[0][1] = 1.0;
    SPHERE_POLE[0][2] = 0.0;
    
    SPHERE_POLE[1][0] = 0.0;
    SPHERE_POLE[1][1] = -1.0;
    SPHERE_POLE[1][2] = 0.0;
    
    row = 0;
    
    the = dthe;
    
    for(i=0;i<(n-1);i++)
    {
	
      phi = 0.0;
      col = 0;
      
      for(j=0;j<=nn;j++)
      {
	p[0] = 0.0;
	p[1] = 1.0;
	p[2] = 0.0;
	rotation_x(&p[0], &p[1], &p[2], the);
	rotation_y(&p[0], &p[1], &p[2], phi);
	SPHERE_POINT[row][col][0] = p[0];
	SPHERE_POINT[row][col][1] = p[1];
	SPHERE_POINT[row][col][2] = p[2];
	col++;
	phi += dphi;
      }
      row++;
      the += dthe;
    }
  }

  void drawLightSphere(double x, double y, double z, double r, double cr, double cg, double cb)
  {
    int i, j, n, m;
    double p[3];
    n = SPHERE_DIV_NUM;
    m = 2*SPHERE_DIV_NUM;
    glColor3d(cr, cg, cb);
    
    for(i=0;i<(n-2);i++)
    {
      for(j=0;j<m;j++)
      {
	glBegin(GL_TRIANGLE_FAN);
	p[0] = x + r*SPHERE_POINT[i][j][0];
	p[1] = y + r*SPHERE_POINT[i][j][1];
	p[2] = z + r*SPHERE_POINT[i][j][2];
	
	glVertex3dv(p);
	p[0] = x + r*SPHERE_POINT[i+1][j][0];
	p[1] = y + r*SPHERE_POINT[i+1][j][1];
	p[2] = z + r*SPHERE_POINT[i+1][j][2];

	glVertex3dv(p);
	p[0] = x + r*SPHERE_POINT[i+1][j+1][0];
	p[1] = y + r*SPHERE_POINT[i+1][j+1][1];
	p[2] = z + r*SPHERE_POINT[i+1][j+1][2];
	
	glVertex3dv(p);
	p[0] = x + r*SPHERE_POINT[i][j+1][0];
	p[1] = y + r*SPHERE_POINT[i][j+1][1];
	p[2] = z + r*SPHERE_POINT[i][j+1][2];
	
	glVertex3dv(p);
	glEnd();
      }
    }
    
    glBegin(GL_TRIANGLE_FAN);
    p[0] = x + r*SPHERE_POLE[0][0];
    p[1] = y + r*SPHERE_POLE[0][1];
    p[2] = z + r*SPHERE_POLE[0][2];
    
    glVertex3dv(p);
    for(j=0;j<=m;j++)
    {
      p[0] = x + r*SPHERE_POINT[0][j][0];
      p[1] = y + r*SPHERE_POINT[0][j][1];
      p[2] = z + r*SPHERE_POINT[0][j][2];
      glVertex3dv(p);
    }
    glEnd();
    
    
    glBegin(GL_TRIANGLE_FAN);
    p[0] = x + r*SPHERE_POLE[1][0];
    p[1] = y + r*SPHERE_POLE[1][1];
    p[2] = z + r*SPHERE_POLE[1][2];
    
    glVertex3dv(p);
    for(j=0;j<=m;j++)
    {
      p[0] = x + r*SPHERE_POINT[SPHERE_DIV_NUM-2][j][0];
      p[1] = y + r*SPHERE_POINT[SPHERE_DIV_NUM-2][j][1];
      p[2] = z + r*SPHERE_POINT[SPHERE_DIV_NUM-2][j][2];
      glVertex3dv(p);
    }
    glEnd();
    
  }
  
  void drawLightWireSphere(double x, double y, double z, double r, double cr, double cg, double cb)
  {
    int i, j, n, m;
    double tp1[3];
    n = SPHERE_DIV_NUM;
    m = 2*SPHERE_DIV_NUM;
    glColor3d(cr, cg, cb);
    
    for(j=0;j<=m;j++)
    {
      glBegin(GL_LINE_STRIP);
      tp1[0] = x + r*SPHERE_POLE[0][0];
      tp1[1] = y + r*SPHERE_POLE[0][1];
      tp1[2] = z + r*SPHERE_POLE[0][2];

      glVertex3dv(tp1);
      for(i=0;i<(n-1);i++)
      {
	tp1[0] = x + r*SPHERE_POINT[i][j][0];
	tp1[1] = y + r*SPHERE_POINT[i][j][1];
	tp1[2] = z + r*SPHERE_POINT[i][j][2];
	glVertex3dv(tp1);
      }
      
      tp1[0] = x + r*SPHERE_POLE[1][0];
      tp1[1] = y + r*SPHERE_POLE[1][1];
      tp1[2] = z + r*SPHERE_POLE[1][2];

      glVertex3dv(tp1);
      glEnd();
    }
    
    for(i=0;i<(n-1);i++)
    {
      glBegin(GL_LINE_STRIP);
      for(j=0;j<=m;j++)
      {
    	tp1[0] = x + r*SPHERE_POINT[i][j][0];
    	tp1[1] = y + r*SPHERE_POINT[i][j][1];
    	tp1[2] = z + r*SPHERE_POINT[i][j][2];
    	glVertex3dv(tp1);
      }
      glEnd();
    }
  }

  void drawRegularOctahedron(double x, double y, double z, double r, double cr, double cg, double cb)
  {
    double Np[3], Sp[3], p1[3], p2[3], p3[3], p4[3];
    Np[0] = x;
    Np[1] = y + r;
    Np[2] = z;
    
    Sp[0] = x;
    Sp[1] = y - r;
    Sp[2] = z;
    
    p1[0] = x + r;
    p1[1] = y;
    p1[2] = z;
    
    p2[0] = x;
    p2[1] = y;
    p2[2] = z + r;
    
    p3[0] = x - r;
    p3[1] = y;
    p3[2] = z;
    
    p4[0] = x;
    p4[1] = y;
    p4[2] = z - r;
    
    glColor3d(cr, cg, cb);
  
    glBegin(GL_TRIANGLE_FAN);
    glVertex3dv(Np);
    glVertex3dv(p1);
    glVertex3dv(p2);
    glVertex3dv(p3);
    glVertex3dv(p4);
    glVertex3dv(p1);
    glEnd();
    
    glBegin(GL_TRIANGLE_FAN);
    glVertex3dv(Sp);
    glVertex3dv(p1);
    glVertex3dv(p2);
    glVertex3dv(p3);
    glVertex3dv(p4);
    glVertex3dv(p1);
    glEnd();
  }

  void drawSolidCube(double x, double y, double z, double lx, double ly, double lz, double r, double g, double b)
  {
    double p1[3], p2[3], p3[3], p4[3], p5[3], p6[3], p7[3];
    
    p2[0] = x + lx*(VERTEX[1][0] - 0.5);
    p2[1] = y + ly*(VERTEX[1][1] - 0.5);
    p2[2] = z + lz*(VERTEX[1][2] - 0.5);
    
    p3[0] = x + lx*(VERTEX[2][0] - 0.5);
    p3[1] = y + ly*(VERTEX[2][1] - 0.5);
    p3[2] = z + lz*(VERTEX[2][2] - 0.5);
    
    p4[0] = x + lx*(VERTEX[3][0] - 0.5);
    p4[1] = y + ly*(VERTEX[3][1] - 0.5);
    p4[2] = z + lz*(VERTEX[3][2] - 0.5);
    
    p5[0] = x + lx*(VERTEX[7][0] - 0.5);
    p5[1] = y + ly*(VERTEX[7][1] - 0.5);
    p5[2] = z + lz*(VERTEX[7][2] - 0.5);
  
    p6[0] = x + lx*(VERTEX[4][0] - 0.5);
    p6[1] = y + ly*(VERTEX[4][1] - 0.5);
    p6[2] = z + lz*(VERTEX[4][2] - 0.5);
    
    p7[0] = x + lx*(VERTEX[5][0] - 0.5);
    p7[1] = y + ly*(VERTEX[5][1] - 0.5);
    p7[2] = z + lz*(VERTEX[5][2] - 0.5);
    
    glColor3d(r, g, b);
    glBegin(GL_TRIANGLE_FAN);
    
    p1[0] = x + lx*(VERTEX[0][0] - 0.5);
    p1[1] = y + ly*(VERTEX[0][1] - 0.5);
    p1[2] = z + lz*(VERTEX[0][2] - 0.5);
    
    glVertex3dv(p1);
    glVertex3dv(p2);
    glVertex3dv(p3);
    glVertex3dv(p4);
    glVertex3dv(p5);
    glVertex3dv(p6);
    glVertex3dv(p7);
    glVertex3dv(p2);
    
    glEnd();
    
    glBegin(GL_TRIANGLE_FAN);
    
    p1[0] = x + lx*(VERTEX[6][0] - 0.5);
    p1[1] = y + ly*(VERTEX[6][1] - 0.5);
    p1[2] = z + lz*(VERTEX[6][2] - 0.5);
    
    glVertex3dv(p1);
    glVertex3dv(p2);
    glVertex3dv(p3);
    glVertex3dv(p4);
    glVertex3dv(p5);
    glVertex3dv(p6);
    glVertex3dv(p7);
    glVertex3dv(p2);
    
    glEnd();
  }
  
  void drawWireField()
  {
    int c, w, length;
    double x, y, z;
  
    //get_block_list_length(&length);
    length = AppFM.block_list.size();
    
    for(c=0;c<length;c++)
    {
      //get_block_list(&c, &w, &x, &y, &z, &cr, &cg, &cb);
      w  = AppFM.block_list[c].wall;
      x  = AppFM.block_list[c].x;
      y  = AppFM.block_list[c].y;
      z  = AppFM.block_list[c].z;
      
      if(SHOW_FIELD_MODE==0)
      {
	if(w==1)
	{
	  drawWireCube(x, y+0.01, z, 1.0, 1.0, 1.0, 0.1, 0.1, 0.9);
	}
      }
      else if(SHOW_FIELD_MODE==1)
      {
	if(w==1)
	{
	  drawWireCube(x, y+0.01, z, 1.0, 1.0, 1.0, 0.1, 0.1, 0.9);
	}
      }
      else if(SHOW_FIELD_MODE==2)
      {
      }
    }
    
  }

  void drawNewBlock()
  {
    double x, y, z;

    x  = AppFM.newBlock.x;
    y  = AppFM.newBlock.y;
    z  = AppFM.newBlock.z;

    drawWireCube(x, y, z, 1.0, 1.0, 1.0, 0.9, 0.1, 0.1);
    
  }

  void drawBlockToBeDeleted()
  {
    double x, y, z;

    x  = AppFM.blockToBeDeleted.x;
    y  = AppFM.blockToBeDeleted.y;
    z  = AppFM.blockToBeDeleted.z;

    drawSolidCube(x, y, z, 1.1, 1.1, 1.1, 0.9, 0.1, 0.1);
    
  }
  

  void drawSolidField()
  {
    int c, w, length;
    double x, y, z, cr, cg, cb, cc;

    int FIELD_SIZE_X = AppFM.FIELD_SIZE_X;
    int FIELD_SIZE_Y = AppFM.FIELD_SIZE_Y;
    int FIELD_SIZE_Z = AppFM.FIELD_SIZE_Z;
    int FIELD_CENTER_X = 0;
    int FIELD_CENTER_Y = FIELD_SIZE_Y/4;
    int FIELD_CENTER_Z = 0;

    std::vector<std::vector<double>> hmap(FIELD_SIZE_X, std::vector<double>(FIELD_SIZE_Z));
    for(int i=0; i<FIELD_SIZE_X; i++)
    {
      for(int k=0; k<FIELD_SIZE_Z; k++)
      {
	hmap[i][k] = -FIELD_CENTER_Y;
      }
    }
    
    //get_block_list_length(&length);
    length = AppFM.block_list.size();

    if(SHOW_FIELD_MODE==0)
    {

      for(c=0;c<length;c++)
      {
	//get_block_list(&c, &w, &x, &y, &z, &cr, &cg, &cb);
	w  = AppFM.block_list[c].wall;
	x  = AppFM.block_list[c].x;
	y  = AppFM.block_list[c].y;
	z  = AppFM.block_list[c].z;
	cr = AppFM.block_list[c].cr;
	cg = AppFM.block_list[c].cg;
	cb = AppFM.block_list[c].cb;

	if(w==1)
	{
	  drawSolidCube(x, y, z, 1.0, 1.0, 1.0, 0.99, 0.99, 0.99);
	}

      }
    }
    else if(SHOW_FIELD_MODE==1)
    {

      for(c=0;c<length;c++)
      {
	//get_block_list(&c, &w, &x, &y, &z, &cr, &cg, &cb);
	w  = AppFM.block_list[c].wall;
	x  = AppFM.block_list[c].x;
	y  = AppFM.block_list[c].y;
	z  = AppFM.block_list[c].z;
	cr = AppFM.block_list[c].cr;
	cg = AppFM.block_list[c].cg;
	cb = AppFM.block_list[c].cb;
	
	if(w==1)
	{
	  drawSolidCube(x, y, z, 1.0, 1.0, 1.0, 0.1, 0.1, 0.1);
	}

      }
    }
    else if(SHOW_FIELD_MODE==2)
    {

      for(c=0;c<length;c++)
      {
	w  = AppFM.block_list[c].wall;
	x  = AppFM.block_list[c].x;
	y  = AppFM.block_list[c].y;
	z  = AppFM.block_list[c].z;
	if(w==1)
	{
	  int i, k; 
	  i = round(x + FIELD_SIZE_X/2 - FIELD_CENTER_X);
	  k = round(z + FIELD_SIZE_Z/2 - FIELD_CENTER_Z);
	  if(hmap[i][k]<y)
	  {
	    hmap[i][k] = y - 0.5;
	  }
	}
      }
	
      for(c=0;c<length;c++)
      {
	//get_block_list(&c, &w, &x, &y, &z, &cr, &cg, &cb);
	w  = AppFM.block_list[c].wall;
	x  = AppFM.block_list[c].x;
	y  = AppFM.block_list[c].y;
	z  = AppFM.block_list[c].z;
	cr = AppFM.block_list[c].cr;
	cg = AppFM.block_list[c].cg;
	cb = AppFM.block_list[c].cb;
	
	if(w==1)
	{

	  int i, k;
	  
	  i = round(x + FIELD_SIZE_X/2 - FIELD_CENTER_X);
	  //j = round(y + FIELD_SIZE_Y/2 - FIELD_CENTER_Y);
	  k = round(z + FIELD_SIZE_Z/2 - FIELD_CENTER_Z);

	  if(hmap[i][k]<y)
	  {
	    //c = 0.5+0.5*(FIELD_CENTER_Y+y)/FIELD_SIZE_Y;
	    cc = 0.25+0.75*(FIELD_CENTER_Y+y)/32.0;
	    //cc = (FIELD_CENTER_Y+y)/128.0;
	    if(cc<0.0)
	    {
	      cc = 0.0;
	    }
	    else if(1.0<cc)
	    {
	      cc = 1.0;
	    }
	    drawSolidCube(x, -1.0, z, 1.0, 1.0, 1.0, cc, cc, cc);
	  
	  }

	}
	
      }
      
    }
    else
    {
      for(c=0;c<length;c++)
      {
	//get_block_list(&c, &w, &x, &y, &z, &cr, &cg, &cb);
	w  = AppFM.block_list[c].wall;
	x  = AppFM.block_list[c].x;
	y  = AppFM.block_list[c].y;
	z  = AppFM.block_list[c].z;
	cr = AppFM.block_list[c].cr;
	cg = AppFM.block_list[c].cg;
	cb = AppFM.block_list[c].cb;

	drawSolidCube(x, y, z, 1.0, 1.0, 1.0, cr, cg, cb);
      }
    }

  }

  
  void drawCell()
  {
    int si, i, sum_ccf, itf;
    double x, y, z, r, cr, cg, cb, e, oe;
    double next_max_energy;
    double age, next_max_age; 
  
    if(SHOW_CELL_MODE==5)
    {
      return;
    }

    next_max_energy = -1.0;
  
    //get_calc_cll(&CALC_CLL);
    if(SHOW_IT_FLAG==0)
    {
      si = 1;
    }
    else
    {
      si = 0;
    }

    //for(i=si;i<CALC_CLL;i++)
    for(i=si;i<AppCM.num;i++)
    {

      if(AppCM.aliveFlagList[i]==0)
      {
	continue;
      }
      
      //get_calc_cl(&i, &ID, &x, &y, &z, &r, &cr, &cg, &cb, &e, &sum_ccf, &itf, &oe, &iage);
      x  = AppCM.cells[i].x;
      y  = AppCM.cells[i].y;
      z  = AppCM.cells[i].z;
      r  = AppCM.cells[i].r;
      cr = AppCM.cells[i].cr;
      cg = AppCM.cells[i].cg;
      cb = AppCM.cells[i].cb;
      e  = AppCM.cells[i].E;
      sum_ccf = std::accumulate(AppCM.cells[i].CCF.begin(), AppCM.cells[i].CCF.end(), 0);
      itf = AppCM.cells[i].INFO_TRANS_F;
      oe = AppCM.cells[i].out_data[3*2+2-1];
      age = (float)AppCM.cells[i].age;

      if(itf==0)
      {

	if(SHOW_CELL_MODE==0)
	{
	  drawLightWireSphere(x,  y,  z,  r, cr, cg, cb);
	}
	else if(SHOW_CELL_MODE==1)
	{
	  drawLightSphere(x,  y,  z,  r, cr,  cg, cb);
	}
	else if(SHOW_CELL_MODE==2)
	{
	  if(sum_ccf<=1)
	  {
	    drawLightSphere(x,  y,  z,  r, cr,  cg, cb);
	  }
	}
	else if(SHOW_CELL_MODE==3)
	{

	  if(i!=0 && next_max_energy<e)
	  {
	    next_max_energy = e;
	  }
	  e = e/MAX_ENERGY;
	  if(1.0<e)
	  {
	    e = 1.0;
	  }
	  else if(e<0.0)
	  {
	    e = 0.0;
	  }
	
	  if(e<0.5)
	  {
	    cr = 0.0;
	    cg = 2.0*e;
	    cb = 1.0-2.0*e;
	  }
	  else
	  {
	    cr = 2.0*e-1.0;
	    cg = 2.0-2.0*e;
	    cb = 0.0;
	  }
	  drawLightWireSphere(x,  y,  z,  r, cr, cg, cb);
	}
	else if(SHOW_CELL_MODE==4)
	{
	  if(0<sum_ccf)
	  {
	    drawLightSphere(x,  y,  z,  r, cr,  cg, cb);
	  }
	}
	else if(SHOW_CELL_MODE==6)
	{
	  if(sum_ccf==0)
	  {
	    drawLightSphere(x,  y,  z,  r, cr,  cg, cb);
	  }
	}      
	else if(SHOW_CELL_MODE==7)
	{
	  if(i!=0 && next_max_energy<e)
	  {
	    next_max_energy = e;
	  }
	  e = e/MAX_ENERGY;
	  if(1.0<e)
	  {
	    e = 1.0;
	  }
	  else if(e<0.0)
	  {
	    e = 0.0;
	  }
	
	  if(e<0.5)
	  {
	    cr = 0.0;
	    cg = 2.0*e;
	    cb = 1.0-2.0*e;
	  }
	  else
	  {
	    cr = 2.0*e-1.0;
	    cg = 2.0-2.0*e;
	    cb = 0.0;
	  }
	
	  if(sum_ccf==0)
	  {
	    drawLightWireSphere(x,  y,  z,  r, cr, cg, cb);
	  }
	}
	else if(SHOW_CELL_MODE==8)
	{
	  if(sum_ccf==0)
	  {
	    drawLightSphere(x,  y,  z,  r, 0.9, 0.9, 0.9);
	  }
	  else
	  {
	    if(0.0<oe)
	    {
	      drawLightSphere(x,  y,  z,  r, oe,  0.0, 0.0);
	    }
	    else
	    {
	      drawLightSphere(x,  y,  z,  r, 0.0,  0.0, 1.0);
	    }
	  }
	}      
	else if(SHOW_CELL_MODE==9)
	{
	  if(i!=0 && next_max_age<age)
	  {
	    next_max_age = age;
	  }
	  age = log(age)/log(MAX_AGE);
	  if(1.0<age)
	  {
	    age = 1.0;
	  }
	  else if(age<0.0)
	  {
	    age = 0.0;
	  }
	
	  if(age<0.5)
	  {
	    cr = 0.0;
	    cg = 2.0*age;
	    cb = 1.0-2.0*age;
	  }
	  else
	  {
	    cr = 2.0*age-1.0;
	    cg = 2.0-2.0*age;
	    cb = 0.0;
	  }
	  drawLightWireSphere(x,  y,  z,  r, cr, cg, cb);
	}
      }
      else
      {
	if(SHOW_IT_FLAG==1)
	{
	  drawRegularOctahedron(x,  y,  z,  r, cr,  cg, cb);
	}
      }
      
    }

    if(SHOW_CELL_MODE==3 || SHOW_CELL_MODE==7)
    {
      MAX_ENERGY = next_max_energy;
    }
    
    if(SHOW_CELL_MODE==9)
    {
      MAX_AGE = next_max_age;
    }
  }

  void drawSelectedCell()
  {
    double x, y, z, r;

    if(CURRENT_SELECTED_CELL_INDEX!=-1 && SELECTION_MODE==1)
    {
      x  = AppCM.cells[CURRENT_SELECTED_CELL_INDEX].x;
      y  = AppCM.cells[CURRENT_SELECTED_CELL_INDEX].y;
      z  = AppCM.cells[CURRENT_SELECTED_CELL_INDEX].z;
      r  = AppCM.cells[CURRENT_SELECTED_CELL_INDEX].r;

      drawLightWireSphere(x,  y,  z,  1.1*r, 0.9, 0.1, 0.1);
    }
    
    for(long unsigned int ii=0; ii<AppCM.selectedCellIndices.size(); ii++)
    {
      int index = AppCM.selectedCellIndices[ii];

      x  = AppCM.cells[index].x;
      y  = AppCM.cells[index].y;
      z  = AppCM.cells[index].z;
      r  = AppCM.cells[index].r;

      drawLightWireSphere(x,  y,  z,  1.1*r, 0.9, 0.1, 0.1);
    }

    if(AppCM.lastSelectedCellIndex!=-1)
    {

      x  = AppCM.cells[AppCM.lastSelectedCellIndex].x;
      y  = AppCM.cells[AppCM.lastSelectedCellIndex].y;
      z  = AppCM.cells[AppCM.lastSelectedCellIndex].z;
      r  = AppCM.cells[AppCM.lastSelectedCellIndex].r;

      drawLightWireSphere(x,  y,  z,  1.2*r, 0.1, 0.1, 0.9);
    }

      
  }

  
  void drawSpring()
  {
    int kLen;
    //int id1, id2;
    int i1, i2;
    //double r1, r2
    double x1, y1, z1, cr1, cg1, cb1;
    double x2, y2, z2, cr2, cg2, cb2;
    double p1[3], p2[3];
    GLfloat width;
    
    glGetFloatv(GL_LINE_WIDTH , &width);
    glLineWidth(5);
    kLen = AppCM.kList.size();

    for(int i=0; i<kLen; i++)
    {
      //get_cc_list(&i, &id1, &id2);
      //if(id1<=-1 || id2<=-1)
      //{
      //	//printf("id1=%d, id2=%d\n", id1, id2);
      //	break;
      //}
      //get_cell(&id1, &e1, &x1, &y1, &z1, &r1, &cr1, &cg1, &cb1);
      //get_cell(&id2, &e2, &x2, &y2, &z2, &r2, &cr2, &cg2, &cb2);

      //id1 = AppCM.kList[i][0];
      //id2 = AppCM.kList[i][1];

      //std::cout << id1 << ", " << id2 << std::endl;
      
      //i1  = AppCM.idDict[id1];
      //i2  = AppCM.idDict[id2];

      i1  = AppCM.kList[i][0];
      i2  = AppCM.kList[i][1];
      
      x1  = AppCM.cells[i1].x;
      y1  = AppCM.cells[i1].y;
      z1  = AppCM.cells[i1].z;
      //r1  = AppCM.cells[i1].r;
      cr1 = AppCM.cells[i1].cr;
      cg1 = AppCM.cells[i1].cg;
      cb1 = AppCM.cells[i1].cb;

      x2  = AppCM.cells[i2].x;
      y2  = AppCM.cells[i2].y;
      z2  = AppCM.cells[i2].z;
      //r2  = AppCM.cells[i2].r;
      cr2 = AppCM.cells[i2].cr;
      cg2 = AppCM.cells[i2].cg;
      cb2 = AppCM.cells[i2].cb;
      
      p1[0] = x1;
      p1[1] = y1;
      p1[2] = z1;
      p2[0] = x2;
      p2[1] = y2;
      p2[2] = z2;
      glColor3d((cr1 + cr2)/2.0, (cg1 + cg2)/2.0, (cb1 + cb2)/2.0);
      glBegin(GL_LINES);
      glVertex3dv(p1);
      glVertex3dv(p2);
      glEnd();
      
    }
    glLineWidth(width);
  }

  void display(GLFWwindow *window)
  {
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);	

    glEnable(GL_DEPTH_TEST);
    glLoadIdentity();
    gluLookAt(LOOKAT_EX, LOOKAT_EY, LOOKAT_EZ, LOOKAT_CX, LOOKAT_CY, LOOKAT_CZ, 0.0, 1.0, 0.0);
    glLineWidth(2);

    drawAxis();
    
    drawSolidField();
    drawWireField();  
    drawCell();
    drawSpring();

    double xpos, ypos;
    glfwGetCursorPos(window, &xpos, &ypos);

    int renderBufferWidth, renderBufferHeight;
    glfwGetFramebufferSize(window, &renderBufferWidth, &renderBufferHeight);
    if (WIN_WIDTH > 0 && WIN_HEIGHT > 0)
    {
      xpos = (renderBufferWidth/WIN_WIDTH)*xpos;
      ypos = (renderBufferHeight/WIN_HEIGHT)*ypos;
    }
    
    double worldX, worldY, worldZ;
    GLint viewport[4];
    GLdouble modelview[16];
    GLdouble projection[16];
    glGetDoublev(GL_MODELVIEW_MATRIX, modelview);
    glGetDoublev(GL_PROJECTION_MATRIX, projection);
    glGetIntegerv(GL_VIEWPORT, viewport);
    gluUnProject(xpos, viewport[3]-ypos, 0, modelview, projection, viewport, &worldX, &worldY, &worldZ);

    double e2mp[3];
    
    e2mp[0] = worldX - LOOKAT_EX;
    e2mp[1] = worldY - LOOKAT_EY;
    e2mp[2] = worldZ - LOOKAT_EZ;

    if(!CURSOR_ON_ImGuiWINDOW)
    {
      if(SELECTION_MODE==1)
      {
        CURRENT_SELECTED_CELL_INDEX = AppCM.getIntersectedCellIndex(LOOKAT_EX, LOOKAT_EY, LOOKAT_EZ, e2mp, SHOW_IT_FLAG);
        if(MOTION_LEFT_BUTTON_FLAG==1 && CURRENT_SELECTED_CELL_INDEX!=-1)
        {
	  AppCM.addSelectedCell(CURRENT_SELECTED_CELL_INDEX);
	  TRANSLATE_X = 0.0;
	  TRANSLATE_Y = 0.0;
	  TRANSLATE_Z = 0.0;
        }
      }
      else if(SELECTION_MODE==2)
      {
        CURRENT_SELECTED_CELL_INDEX = AppCM.getIntersectedCellIndex(LOOKAT_EX, LOOKAT_EY, LOOKAT_EZ, e2mp, SHOW_IT_FLAG);
        if(MOTION_LEFT_BUTTON_FLAG==1 && CURRENT_SELECTED_CELL_INDEX!=-1)
        {
	  AppCM.removeSelectedCell(CURRENT_SELECTED_CELL_INDEX);
	  TRANSLATE_X = 0.0;
	  TRANSLATE_Y = 0.0;
	  TRANSLATE_Z = 0.0;
        }
      }
      else if(SELECTION_MODE==3)
      {
        AppFM.detectNewCandidateBlock(LOOKAT_EX, LOOKAT_EY, LOOKAT_EZ, e2mp);
        if(AppFM.newBlockFlag==1)
        {
	  drawNewBlock();
	  if(MOTION_LEFT_BUTTON_FLAG==1)
	  {
	    MOTION_LEFT_BUTTON_FLAG = 0;
	    AppFM.addBlock();
	  }
        }
      }
      else if(SELECTION_MODE==4)
      {
        AppFM.detectBlockToBeDeleted(LOOKAT_EX, LOOKAT_EY, LOOKAT_EZ, e2mp);
        if(AppFM.blockToBeDeletedFlag==1)
        {
	  drawBlockToBeDeleted();
	  if(MOTION_LEFT_BUTTON_FLAG==1)
	  {
	    MOTION_LEFT_BUTTON_FLAG = 0;
	    AppFM.removeBlock();
	  }
        }
      }
    }
    else
    {
      CURRENT_SELECTED_CELL_INDEX = -1;
    }

    if(SELECTION_MODE==1 || SELECTION_MODE==2)
    {
      drawSelectedCell();
    }
  
  }
  
  static void rotation_x(double *x, double *y, double *z, double rad)
  {
    double tmpY, tmpZ;
    
    tmpY = *y;
    tmpZ = *z;
    
    *y = tmpY*cos(rad) - tmpZ*sin(rad);
    *z = tmpY*sin(rad) + tmpZ*cos(rad);
  }

  static void rotation_y(double *x, double *y, double *z, double rad)
  {
    double tmpX, tmpZ;
    
    tmpX = *x;
    tmpZ = *z;
    
    *x =  tmpX*cos(rad) + tmpZ*sin(rad);
    *z = -tmpX*sin(rad) + tmpZ*cos(rad);
  }
  
  static void rotation_z(double *x, double *y, double *z, double rad)
  {
    double tmpX, tmpY;
    
    tmpX = *x;
    tmpY = *y;
    
    *x = tmpX*cos(rad) - tmpY*sin(rad);
    *y = tmpX*sin(rad) + tmpY*cos(rad);
  }

  void initializeGL()
  {
    
    // 背景色の設定
    //glClearColor(0.9f, 0.9f, 0.9f, 1.0f);
    
    printf("Start setSphere...\n");
    setSphere();
    
    //printf("Start praparat_init...\n");
    //praparat_init();
    
    printf("Start glClearColor...\n");
    glClearColor(1.0, 1.0, 1.0, 1.0);
    
    //printf("Start get_field_size...\n");
    //get_field_size(&FSx, &FSy, &FSz);
    
    //printf("Start get_field_center...\n");
    //get_field_center(&FCx, &FCy, &FCz);
    
    //printf("FSx, FSy, FSz, FCx, FCy, FCz\n");
    //printf("  %d, %d, %d, %d, %d, %d\n", FSx, FSy, FSz, FCx, FCy, FCz);
    
  }
  
  int Main() {
    char winTitle[256];
    char filename[256];
    //int n=1;
    //int m=1;
    int world_step = 0;

    if (glfwInit() == GL_FALSE)
    {
      fprintf(stderr, "Initialization failed!\n");
      return 1;
    }
    
    GLFWwindow *window = glfwCreateWindow(WIN_WIDTH, WIN_HEIGHT, WIN_TITLE,
					  NULL, NULL);
    if (window == NULL)
    {
      fprintf(stderr, "Window creation failed!");
      glfwTerminate();
      return 1;
    }

    
    {
      glfwSetWindowSizeCallback(window, windowSizeCB);
      
      glfwSetMouseButtonCallback(window, mouseButtonCB);
      glfwSetCursorPosCallback(window, mousePosCB);
      glfwSetScrollCallback(window, mouseScrollCB);
      
      glfwSetKeyCallback(window, keyFunCB);
      glfwSetCharCallback(window, charFunCB);
      
      glfwSetDropCallback(window, dropCB);
    }
    
    glfwMakeContextCurrent(window);
        
    initializeGL();
    
    glfwSwapInterval(1);
        
    //pthread_t pt_praparat;
    //pthread_create(&pt_praparat, NULL, pstep, &n);
        
    //< --------------------------------------- >
    //< ----------------imgui------------------ >      
    // Setup Dear ImGui context
    IMGUI_CHECKVERSION();
    ImGui::CreateContext();
    ImGuiIO& io = ImGui::GetIO(); (void)io;

    ImGui::StyleColorsDark();

    ImGui_ImplGlfw_InitForOpenGL(window, true);
    
    //ImGui_ImplOpenGL3_Init(glsl_version);
    ImGui_ImplOpenGL2_Init();
    //< ----------------imgui------------------ >
    //< --------------------------------------- >
        
    int newFieldSizeX;
    int newFieldSizeY;
    int newFieldSizeZ;
    char newFieldSizeX_buf[32] = "32";
    char newFieldSizeY_buf[32] = "64";
    char newFieldSizeZ_buf[32] = "32";

    sscanf(newFieldSizeX_buf, "%d", &newFieldSizeX);
    sscanf(newFieldSizeY_buf, "%d", &newFieldSizeY);
    sscanf(newFieldSizeZ_buf, "%d", &newFieldSizeZ);
    
    bool showCellDetailsWindow = false;
    ImVec2 dialogMinSize = ImVec2(300.0, 150.0);
    
    while (glfwWindowShouldClose(window) == GL_FALSE)
    {
      
      //ImGui_ImplOpenGL3_NewFrame();
      ImGui_ImplOpenGL2_NewFrame();
      ImGui_ImplGlfw_NewFrame();
      ImGui::NewFrame();








      {

	ImGui::Begin("Main Window");                          
	CURSOR_ON_ImGuiWINDOW = ImGui::IsWindowHovered(ImGuiHoveredFlags_AllowWhenBlockedByActiveItem |
						       ImGuiHoveredFlags_ChildWindows |
						       ImGuiHoveredFlags_AllowWhenBlockedByPopup);
	
	if(SELECTION_MODE==0)
	{
	  ImGui::Text("MODE => --");
	}	
	else if(SELECTION_MODE==1)
	{
	  ImGui::Text("MODE => Select Single Cell Mode");
	}
	else if(SELECTION_MODE==2)
	{
	  ImGui::Text("MODE => Deselect Single Cell Mode");
	}
	else if(SELECTION_MODE==3)
	{
	  ImGui::Text("MODE => Add One Block Mode");
	}
	else if(SELECTION_MODE==4)
	{
	  ImGui::Text("MODE => Delete One Block Mode");
	}

	ImGui::End();
      }











      
      {

	ImGui::Begin("Control Cell Window");                          
	CURSOR_ON_ImGuiWINDOW = CURSOR_ON_ImGuiWINDOW ||
	  ImGui::IsWindowHovered(ImGuiHoveredFlags_AllowWhenBlockedByActiveItem |
				 ImGuiHoveredFlags_ChildWindows |
				 ImGuiHoveredFlags_AllowWhenBlockedByPopup);
	
	ImGui::Checkbox("Cell Details Window", &showCellDetailsWindow);

	ImGui::Text("Read:");
	
	if (ImGui::Button("Cells"))
	{
	  ImGuiFileDialog::Instance()->OpenDialog("ReadCellsDlgKey", "Choose File", ".*", ".");
	  SCROLL_ACTIVE  = 0;
	  SELECTION_MODE = 0;
	}
	
	if (ImGuiFileDialog::Instance()->Display("ReadCellsDlgKey",
						 ImGuiWindowFlags_NoCollapse, dialogMinSize)) 
	{
	  if (ImGuiFileDialog::Instance()->IsOk())
	  {
	    std::string filePathName = ImGuiFileDialog::Instance()->GetFilePathName();
	    std::string filePath = ImGuiFileDialog::Instance()->GetCurrentPath();
	    printf("filePathName=%s\n", filePathName.c_str());
	    printf("filePath    =%s\n", filePath.c_str());
	    AppCM.readFile(filePathName);
	  }
	  
	  ImGuiFileDialog::Instance()->Close();
	  SCROLL_ACTIVE = 1;
	}
		
	ImGui::Spacing();

	ImGui::Text("Selection:");

	int nosc = AppCM.selectedCellIndices.size();
	ImGui::Text("Number of selected cells = %d", nosc);
	
	if (ImGui::Button("Select All"))
	{
	  AppCM.resetSelectedCellIndices();
	  for(int i=0; i<AppCM.num; i++)
	  {
	    if(AppCM.aliveFlagList[i]==1)
	    {
	      AppCM.addSelectedCell(i);
	    }
	  }
	}

	if (ImGui::Button("Deselect All Selected Cell"))
	{
	  AppCM.resetSelectedCellIndices();
	}

	if (ImGui::Button("Select Connected Cell")) 
	  AppCM.selectConnectedCell(AppCM.lastSelectedCellIndex); 
	
	if (ImGui::Button("Invert Selection"))
	{
	  std::vector<int> sci;
	  for(long unsigned int i=0; i<AppCM.selectedCellIndices.size(); i++)
	  {
	    sci.push_back(AppCM.selectedCellIndices[i]);
	  }

	  AppCM.resetSelectedCellIndices();
	  for(int i=0; i<AppCM.num; i++)
	  {
	    if(AppCM.aliveFlagList[i]==1)
	    {

	      if(std::find(sci.begin(), sci.end(), i)==sci.end())
	      {
		AppCM.addSelectedCell(i);
	      }	      
	    }
	  }
	  AppCM.lastSelectedCellIndex = -1;
	}

	if (ImGui::Button("Select All Photon"))
	{
	  AppCM.resetSelectedCellIndices();
	  for(int i=0; i<AppCM.num; i++)
	  {
	    if(AppCM.cells[i].INFO_TRANS_F==1)
	    {

	      AppCM.addSelectedCell(i);
	    }
	  }
	}
	
	ImGui::Spacing();

	ImGui::Text("Operation:");
	
	if (ImGui::Button("Kill Selected Cell"))
	{
	  CURRENT_SELECTED_CELL_INDEX = -1;
	  AppCM.killSelectedCell();
	}

	ImGui::PushItemWidth(200);
	bool dxb = ImGui::SliderFloat("dx", &TRANSLATE_X, -10.0f, 10.0f); 
	bool dyb = ImGui::SliderFloat("dy", &TRANSLATE_Y, -10.0f, 10.0f); 
	bool dzb = ImGui::SliderFloat("dz", &TRANSLATE_Z, -10.0f, 10.0f); 

	if(dxb || dyb || dzb)
	{
	  std::cout << TRANSLATE_X << ", " << TRANSLATE_Y << ", " << TRANSLATE_Z << std::endl;
	  AppCM.translateSelectedCell(TRANSLATE_X, TRANSLATE_Y, TRANSLATE_Z);
	}

	ImGui::Spacing();
	
	ImGui::Text("Export:");
	
	if (ImGui::Button("Cells*"))
	{
	  ImGuiFileDialog::Instance()->OpenDialog("SaveCellsDlgKey", "Save", ".*", ".");
	  SCROLL_ACTIVE  = 0;
	  KEY_ACTIVE     = 0;
	  SELECTION_MODE = 0;
	}
	
	if (ImGuiFileDialog::Instance()->Display("SaveCellsDlgKey",
						 ImGuiWindowFlags_NoCollapse, dialogMinSize)) 
	{
	  if (ImGuiFileDialog::Instance()->IsOk())
	  {
	    std::string filePathName = ImGuiFileDialog::Instance()->GetFilePathName();
	    std::string filePath = ImGuiFileDialog::Instance()->GetCurrentPath();
	    printf("filePathName=%s\n", filePathName.c_str());
	    printf("filePath    =%s\n", filePath.c_str());
	    AppCM.write(filePathName);
	  }
	  
	  ImGuiFileDialog::Instance()->Close();
	  SCROLL_ACTIVE = 1;
	  KEY_ACTIVE    = 1;
	}	

	ImGui::End();
      }









      {

	ImGui::Begin("Control Field Window");
	CURSOR_ON_ImGuiWINDOW = CURSOR_ON_ImGuiWINDOW ||
	  ImGui::IsWindowHovered(ImGuiHoveredFlags_AllowWhenBlockedByActiveItem |
				 ImGuiHoveredFlags_ChildWindows |
				 ImGuiHoveredFlags_AllowWhenBlockedByPopup);
	
	ImGui::Text("Read:");
	
	if (ImGui::Button("Field"))
	{
	  ImGuiFileDialog::Instance()->OpenDialog("ChooseFieldDlgKey", "Choose File", ".*", ".");
	  SCROLL_ACTIVE  = 0;
	  SELECTION_MODE = 0;
	}

	if (ImGuiFileDialog::Instance()->Display("ChooseFieldDlgKey",
						 ImGuiWindowFlags_NoCollapse, dialogMinSize)) 
	{
	  if (ImGuiFileDialog::Instance()->IsOk())
	  {
	    std::string filePathName = ImGuiFileDialog::Instance()->GetFilePathName();
	    std::string filePath = ImGuiFileDialog::Instance()->GetCurrentPath();
	    printf("filePathName=%s\n", filePathName.c_str());
	    printf("filePath    =%s\n", filePath.c_str());
	    AppFM.read(filePathName);
	  }
	  
	  ImGuiFileDialog::Instance()->Close();
	  SCROLL_ACTIVE = 1;
	}

	ImGui::Spacing();

	ImGui::Text("Creation:");
	
	strcpy(newFieldSizeY_buf, "64");
	ImGui::PushItemWidth(50);
	ImGui::Text("x");
	ImGui::SameLine();
	ImGui::InputText("##newFieldSizeX", newFieldSizeX_buf, IM_ARRAYSIZE(newFieldSizeX_buf));
	ImGui::SameLine();
	ImGui::Text("y");
	ImGui::SameLine();
	ImGui::InputText("##newFieldSizeY", newFieldSizeY_buf, IM_ARRAYSIZE(newFieldSizeY_buf));
	ImGui::SameLine();
	ImGui::Text("z");
	ImGui::SameLine();
	ImGui::InputText("##newFieldSizeZ", newFieldSizeZ_buf, IM_ARRAYSIZE(newFieldSizeZ_buf));
	if (ImGui::Button("Create Flat Field"))
	{
	  sscanf(newFieldSizeX_buf, "%d", &newFieldSizeX);
	  sscanf(newFieldSizeY_buf, "%d", &newFieldSizeY);
	  sscanf(newFieldSizeZ_buf, "%d", &newFieldSizeZ);
	  AppFM.create_flat_field(newFieldSizeX, newFieldSizeY, newFieldSizeZ, 128);
	}
	if (ImGui::Button("Create Random Field"))
	{
	  sscanf(newFieldSizeX_buf, "%d", &newFieldSizeX);
	  sscanf(newFieldSizeY_buf, "%d", &newFieldSizeY);
	  sscanf(newFieldSizeZ_buf, "%d", &newFieldSizeZ);
	  AppFM.create_random_field(newFieldSizeX, newFieldSizeY, newFieldSizeZ, 128);
	}
	
	ImGui::Spacing();
	
	ImGui::Text("Export:");
	
	if (ImGui::Button("Field*"))
	{
	  ImGuiFileDialog::Instance()->OpenDialog("SaveFieldDlgKey", "Save", ".*", ".");
	  SCROLL_ACTIVE  = 0;
	  KEY_ACTIVE     = 0;
	  SELECTION_MODE = 0;
	}
	
	if (ImGuiFileDialog::Instance()->Display("SaveFieldDlgKey",
						 ImGuiWindowFlags_NoCollapse, dialogMinSize)) 
	{
	  if (ImGuiFileDialog::Instance()->IsOk())
	  {
	    std::string filePathName = ImGuiFileDialog::Instance()->GetFilePathName();
	    std::string filePath = ImGuiFileDialog::Instance()->GetCurrentPath();
	    printf("filePathName=%s\n", filePathName.c_str());
	    printf("filePath    =%s\n", filePath.c_str());
	    AppFM.write(filePathName);
	  }
	  
	  ImGuiFileDialog::Instance()->Close();
	  SCROLL_ACTIVE = 1;
	  KEY_ACTIVE    = 1;
	}

	ImGui::End();
      }













      
      if (showCellDetailsWindow)
      {
	ImGui::Begin("Cell Details Window", &showCellDetailsWindow);
	CURSOR_ON_ImGuiWINDOW = CURSOR_ON_ImGuiWINDOW ||
	  ImGui::IsWindowHovered(ImGuiHoveredFlags_AllowWhenBlockedByActiveItem |
				 ImGuiHoveredFlags_ChildWindows |
				 ImGuiHoveredFlags_AllowWhenBlockedByPopup);
	
	if(CURRENT_SELECTED_CELL_INDEX!=-1)
	{
	  long unsigned int bookShowMaxLen = 20;
	  auto CODES = AppCM.cells[CURRENT_SELECTED_CELL_INDEX].CODES;
	  int    ID = AppCM.cells[CURRENT_SELECTED_CELL_INDEX].ID;
	  auto book = AppCM.cells[CURRENT_SELECTED_CELL_INDEX].book;
	  auto bm   = AppCM.cells[CURRENT_SELECTED_CELL_INDEX].bookmarker;
	  auto bma  = AppCM.cells[CURRENT_SELECTED_CELL_INDEX].bookmarker_advance;
	  double x  = AppCM.cells[CURRENT_SELECTED_CELL_INDEX].x;
	  double y  = AppCM.cells[CURRENT_SELECTED_CELL_INDEX].y;
	  double z  = AppCM.cells[CURRENT_SELECTED_CELL_INDEX].z;
	  double r  = AppCM.cells[CURRENT_SELECTED_CELL_INDEX].r;
	  double cr = AppCM.cells[CURRENT_SELECTED_CELL_INDEX].cr;
	  double cg = AppCM.cells[CURRENT_SELECTED_CELL_INDEX].cg;
	  double cb = AppCM.cells[CURRENT_SELECTED_CELL_INDEX].cb;
	  double e  = AppCM.cells[CURRENT_SELECTED_CELL_INDEX].E;

	  if(book.size()<bookShowMaxLen)
	  {
	    bookShowMaxLen = book.size();
	  }
	  
	  ImGui::Text("ID = %d", ID);
	  
	  std::stringstream ss;
	  for(long unsigned int i=0; i<bookShowMaxLen; i++)
	  {
	    ss << CODES[book[i]-1];
	  }
	  ss << "...";
	  std::string str = ss.str();
	  ImGui::Text("book = %s", str.c_str());

	  ss.str(""); 
	  for(long unsigned int i=0; i<bm.size(); i++)
	  {
	    ss << CODES[bm[i]-1];
	  }
	  str = ss.str();
	  ImGui::Text("bm = %s", str.c_str());

	  ss.str(""); 
	  for(long unsigned int i=0; i<bma.size(); i++)
	  {
	    ss << CODES[bma[i]-1];
	  }
	  str = ss.str();
	  ImGui::Text("bma = %s", str.c_str());
	  
	  ImGui::Text("x  = %lf", x);
	  ImGui::Text("y  = %lf", y);
	  ImGui::Text("z  = %lf", z);
	  ImGui::Text("r  = %lf", r);
	  ImGui::Text("cr = %lf", cr);
	  ImGui::Text("cg = %lf", cg);
	  ImGui::Text("cb = %lf", cb);
	  ImGui::Text("E  = %lf", e);
	  
	}
	
	ImGui::End();
      }
      
      ImGui::Render();
      
      sprintf(winTitle, "%s (%d)-(%d)", WIN_TITLE, world_step, SHOW_CELL_MODE);
      glfwSetWindowTitle(window, winTitle);
      display(window);

      //ImGui_ImplOpenGL3_RenderDrawData(ImGui::GetDrawData());
      ImGui_ImplOpenGL2_RenderDrawData(ImGui::GetDrawData());

#if defined(__APPLE__)
      resize4apple(window, WIN_WIDTH, WIN_HEIGHT);
#else
      resize(window, WIN_WIDTH, WIN_HEIGHT);
#endif
      
      glfwSwapBuffers(window);
      glfwPollEvents();
      
      if(REC_FLAG==1)
      {
	sprintf(filename, "./pic/%08d.bmp", world_step);
	GLubyte* pixel_data = (GLubyte*)malloc((WIN_WIDTH_PIXEL)*(WIN_HEIGHT_PIXEL)*3*(sizeof(GLubyte)));
	//glReadBuffer( GL_FRONT ); //GL_FRONT:フロントバッファ　GL_BACK:バックバッファ
	glReadBuffer( GL_BACK ); //GL_FRONT:フロントバッファ　GL_BACK:バックバッファ
	glReadPixels(
		     0, 0,
		     WIN_WIDTH_PIXEL, WIN_HEIGHT_PIXEL,
		     GL_RGB,
		     GL_UNSIGNED_BYTE,
		     pixel_data);
	WriteBitmap(filename, pixel_data, WIN_WIDTH_PIXEL, WIN_HEIGHT_PIXEL);
	free(pixel_data);
	//step(&n);
	//step(&m);
      }
    }

    //ImGui_ImplOpenGL3_Shutdown();
    ImGui_ImplOpenGL2_Shutdown();
    ImGui_ImplGlfw_Shutdown();
    ImGui::DestroyContext();    

    return(0);
    
  }

};

//grep
int App::WIN_WIDTH  = 800; 
int App::WIN_HEIGHT = 600; 

int App::WIN_WIDTH_PIXEL  = 0; 
int App::WIN_HEIGHT_PIXEL = 0; 

int App::MOTION_LEFT_BUTTON_FLAG = 0;
int App::MOTION_RIGHT_BUTTON_FLAG = 0;

double App::LOOKAT_EX = 12.699582;
double App::LOOKAT_EY = 16.800000;
double App::LOOKAT_EZ = 7.816028;
double App::LOOKAT_CX = 9.383321;
double App::LOOKAT_CY = 12.996581;
double App::LOOKAT_CZ = 5.934264;

int App::CAMERA_MODE = 0;
int App::SHOW_IT_FLAG = 1;
int App::SHOW_FIELD_MODE = 0;
int App::SHOW_CELL_MODE = 0;
int App::SCROLL_ACTIVE = 1;
int App::KEY_ACTIVE = 1;
int App::SELECTION_MODE = 0;

int App::CURRENT_SELECTED_CELL_INDEX = -1;

double App::SHOW_ENERGY_COEFF = 2.0;

float App::TRANSLATE_X = 0.0;
float App::TRANSLATE_Y = 0.0;
float App::TRANSLATE_Z = 0.0;

int App::BMP_COUNT = 0;

int main(int argc, char **argv) {
  
  App app;
  app.Main();
  return(0);
}
