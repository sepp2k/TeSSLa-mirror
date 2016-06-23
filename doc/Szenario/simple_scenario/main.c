/**
  Simple scenario
**/

//#include <stdio.h>
//#include <unistd.h>

int numbers[20] = {81, 3, 27, 73, 33, 83, 16, 8, 8, 89, 45, 18, 30, 67, 65, 85, 85, 78, 60, 82};

void swap(int* a, int i, int j) {
  int h = a[i];
  a[i] = a[j];
  a[j] = h;
}

void quick_sort(int* a, int lo, int hi){  // Marker 1
  if (lo >= hi) return;

  int i = lo;
  int p = hi;

  while (i<p-1) {
    if (a[i] > a[p]) {
      swap(a, i, p-1);
      swap(a, p-1, p);
      p--;
    } else i++;
  }
  if (a[i] > a[p]) {
    swap(a, i, p);
    p--;
  }

  quick_sort(a, 0, p-1);
  quick_sort(a, p+1, hi);
}

int main(void){

  int array[20];

  int i,j,k = 0;

  for (k = 0; k < 10000; k++) {
    for (j = 0; j < 1000; j++) {
      for (i = 0; i < 20; i++) array[i] = numbers[i];
      quick_sort(array, 0, 19); //Marker 2
      //usleep(500000);
    }
  }
  for (i = 0; i < 19; i++) {
    //printf("%d ", array[i]);
  }
  //printf("%d\n", array[19]);

  //printf("%d\n", counter);

}
