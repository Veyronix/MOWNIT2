#include <stdio.h>
#include <gsl/gsl_sf.h>
#include <gsl/gsl_blas.h>
#include <string.h>
#include <time.h> 
 
void create_marks_csv(char *filename,float a[][3],int n,int m){
 
printf("\n Creating %s.csv file",filename);
 
FILE *fp;
 
int i,j;
 
filename=strcat(filename,".csv");
 
fp=fopen(filename,"w+");
 
fprintf(fp,"n,metoda,czas");
 
for(i=0;i<m;i++){
 
    fprintf(fp,"\n%f",a[i][0]);
 
    for(j=1;j<n;j++)
 
        fprintf(fp,",%f ",a[i][j]);
 
    }
 
fclose(fp);
 
printf("\n %sfile created",filename);
 
}


float** better(float** A, float** B,int aRows,int aCols, int bRows, int bCols){

  float **matrix = (float **)malloc(bCols * sizeof(float*));
  int i,j,k;
  for(i = 0; i < bCols; i++){
    matrix[i] = (float *)malloc(bCols * sizeof(float));    
  }
  for(i = 0; i < bCols; i++){
    for(j = 0; j < bCols; j++){
      matrix[i][j] = 0;
    }
  }
  for(i = 0; i < bCols; i++){
    for(k = 0; k < bCols; k++){
      for(j = 0; j < bCols; j++){
        matrix[i][j] = matrix[i][j] + A[i][k]*B[k][j];
      }
    }
  }
  return matrix;
}

float** naive(float** A, float** B,int aRows,int aCols, int bRows, int bCols){
  float **matrix = (float **)malloc(bCols * sizeof(float*));
    int i,j,k;
    for(i = 0; i < bCols; i++){
      matrix[i] = (float *)malloc(bCols * sizeof(float));    
    }
    for(i = 0; i < bCols; i++){
      for(j = 0; j < bCols; j++){
        matrix[i][j] = 0;
      }
    }
    for(i = 0; i < bCols; i++){
      for(j = 0; j < bCols; j++){
        for(k = 0; k < bCols; k++){
          matrix[i][j] = matrix[i][j] + A[i][k]*B[k][j];
        }
      }
    }
    return matrix;

}

  float* blas_mul(float* A, float* B,int aRows,int aCols, int bRows, int bCols){
    float *matrix = (float *)malloc(bCols*aRows * sizeof(float));
    int i,j,k;
    for(i = 0; i < bCols*aRows; i++){
      matrix[i] = 0;    
    }

    int lda = aCols;
    int ldb = bCols;
    int ldc = bCols;
    cblas_sgemm (CblasRowMajor, 
                CblasNoTrans, CblasNoTrans, aRows, bCols, aCols,
                1.0, A, lda, B, ldb, 0.0, matrix, ldc);

    return matrix;
} 

int main(void)
{
  srand(time(NULL));

  float measurement[300][3];
  int iterator = 0;
  int i,j,l,k;
  for(k = 50; k < 1000; k = k + 100){
    for(l = 0; l < 10; l++){ 
      printf("siema\n");
      float **A = (float **)malloc(k * sizeof(float*));
      for(i = 0; i < k; i++){
        A[i] = (float *)malloc(k * sizeof(float));    
      }
      float **B = (float **)malloc(k * sizeof(float*));
      for(i = 0; i < k; i++){
        B[i] = (float *)malloc(k * sizeof(float));    
      }

      for(i = 0; i < k; i++){
        for(j = 0; j < k; j++){
          A[i][j] = rand();
        }
      }

      for(i = 0; i < k; i++){
        for(j = 0; j < k; j++){
          B[i][j] = rand();
        }
      }

      float *blasA = (float *)malloc(k*k * sizeof(float));
      float *blasB = (float *)malloc(k*k * sizeof(float));
      for(i = 0; i < k*k; i++){
        blasA[i] = rand();
        blasB[i] = rand();
      }

      clock_t t; 
      t = clock(); 
      float** tmp = naive(A,B,k,k,k,k);
      t = clock() - t; 
      measurement[iterator][0] = k;
      measurement[iterator][1] = 1;
      measurement[iterator][2] = ((double)t)/CLOCKS_PER_SEC;
      iterator++;

      t = clock(); 
      float** tmp2 = better(A,B,k,k,k,k);
      t = clock() - t; 
      measurement[iterator][0] = k;
      measurement[iterator][1] = 2;
      measurement[iterator][2] = ((double)t)/CLOCKS_PER_SEC;
      iterator++;

      for(i = 0; i < k;i++){
          free(tmp[i]);
          free(tmp2[i]);
      }
      free(tmp);
      free(tmp2);

      t = clock(); 
      float* tmp3 = blas_mul(blasA,blasB,k,k,k,k);
      t = clock() - t; 
      measurement[iterator][0] = k;
      measurement[iterator][1] = 3;
      measurement[iterator][2] = ((double)t)/CLOCKS_PER_SEC;
      iterator++;

      free(tmp3);
      printf("iteration = %d",iterator);
    }
  }

 
    char str[100];
 
    printf("\n Enter the filename :");
 
    gets(str);
 
  create_marks_csv(str,measurement,3,300);

  return 0;
}

