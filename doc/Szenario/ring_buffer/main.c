/*
  compile with: clang -std=gnu11 -pthreads main.c
  (gnu11 instead of c11 for usleep being available in unistd.h)
*/

#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
#include <stdatomic.h>

#define BUF_SIZE 5
const int NUM_CONS = 3;
const char* CONTROL_PATH = "control";
const char* DATA_PATH = "data";

int channel_main_prod = 0;
int channel_main_consumers = 0;

char ring_buffer[BUF_SIZE];

_Atomic(int) read_idx = 0;
int write_idx = 0;

int buffer_next(int idx){
  //return (idx+1)%BUF_SIZE;
  if (idx == BUF_SIZE-1) 
    return 0;
  else 
    return idx+1;
}

void* producer_main(void* arg) 
{
  printf("Producer: startet.\n");

  //open data stream
  //FILE* data_fp = fopen("data", "r");
  FILE* data_fp = (FILE*) arg;
  if(!data_fp) {
    perror("Data file opening failed");
    pthread_exit(NULL);
  } 
  
  char input;
  while (!feof(data_fp) && channel_main_prod!=1)
  {
    if (buffer_next(write_idx) != read_idx) {
        ring_buffer[write_idx] = getc(data_fp);
        printf("Producer: Value %c written to buffer.\n", ring_buffer[write_idx]);
        write_idx = buffer_next(write_idx);
      }
  }
  
  printf("Producer: terminated.\n");
  pthread_exit(NULL);
}

void process_data(char* data)
{
  usleep(500000);
}

void* consumer_main(void* arg)
{
  long id = (long) arg;
  char data = 0;
  int local_read_idx = 0;
  int next_read_idx = 0;
  
  printf("Consumer %ld: startet.\n", id);  
  
  while (channel_main_consumers != 1 || read_idx!=write_idx)
  {    
    local_read_idx = read_idx;  
    if (local_read_idx != write_idx) {
      data = ring_buffer[local_read_idx];
      next_read_idx = buffer_next(local_read_idx);
      if (atomic_compare_exchange_weak(&read_idx, &local_read_idx, next_read_idx)) {
        //process data
        process_data(&data);
        printf("Consumer %ld: processed data %c\n", id, data);
      }
    }
  }
  
  printf("Consumer %ld: terminated.\n", id);  
  pthread_exit(NULL);
}

pthread_t start_producer(FILE* data_fp)
{
  printf("Control: starting producer.\n");
  pthread_t prod_thr;

  int rc = pthread_create(&prod_thr, NULL, producer_main, (void*) data_fp);
  if (rc){
    printf("Control: ERROR; return code from pthread_create() is %d\n", rc);
    return 0;
  }
  return prod_thr;
}

pthread_t* start_consumers() 
{
  int rc;
  static pthread_t cons_thrds[2];
  
  printf("Control: Starting consumer.\n");

  for(long i=0; i<2; i++)
  {
    rc = pthread_create(&cons_thrds[i], NULL, consumer_main, (void *)i);
    if (rc)
    {
      printf("Control: ERROR; return code from pthread_create() for consumer %ld is %d\n", i, rc);
      return 0;
    }
  }  
  return cons_thrds;
}

void stop_producer(pthread_t prod_thr, int* chan)
{
  printf("Control: Waiting for producer to finish...\n");
  *chan = 1;
  int rc = pthread_join(prod_thr, NULL);
  if (rc) {
    printf("Control: ERROR; return code from pthread_join() is %d\n", rc);
  }
  printf("Control: Producer stopped.\n");
  *chan = 0;
}

void stop_consumers(pthread_t* cons_thrds)
{  
  printf("Control: Waiting for consumers to finish...\n");
  channel_main_consumers = 1;
  int rc;  
  for (int i = 0; i < 2; i++)
  {
    rc = pthread_join(cons_thrds[i], NULL);
    if (rc) {
      printf("Control: ERROR; return code from pthread_join() for consumer %d is %d\n", i, rc);
    }
  }
  channel_main_consumers = 0;
  printf("Control: Consumers stopped.\n");  
}

int main(void)
{
  enum Mode 
  {
    maintenance,
    operation
  } mode = maintenance;
  
  pthread_t producer_thr;
  pthread_t* consumer_thrds;
  
  //open control stream
  FILE* ctrl_fp = fopen("control", "r");
  if(!ctrl_fp) {
    perror("File opening failed");
    return EXIT_FAILURE;
  }
  
  //open data stream
  FILE* data_fp = fopen("data", "r");
  
  char input;
  while ((input = getc(ctrl_fp)) != EOF) {
    printf("Control: input: %c\n", input);
    if (input == 'o' && mode == maintenance) {
      producer_thr = start_producer(data_fp);
      consumer_thrds = start_consumers();
      mode = operation;
      printf("Control: operation mode.\n");
    }
    else if (input == 'm' && mode == operation) {
      stop_producer(producer_thr, &channel_main_prod);
      stop_consumers(consumer_thrds);
      mode = maintenance;
      printf("Control: maintenance mode.\n");
      printf("Control: buffer content = |");
      for (int i=0; i<BUF_SIZE; i++) 
      {
        printf(" %c", ring_buffer[i]);
      }
      printf(" |\n");
      printf("Control: write_idx = %d, read_idx = %d\n", write_idx, read_idx);
    }
  }

  printf("Control: Shutting down.\n");
  if (mode == operation)
  {
    stop_producer(producer_thr, &channel_main_prod);
    stop_consumers(consumer_thrds);
  }
  
  fclose(ctrl_fp);
  fclose(data_fp);
  return 0;
}

