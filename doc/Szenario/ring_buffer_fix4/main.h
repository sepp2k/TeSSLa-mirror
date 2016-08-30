/**
 * Version of ring_buffer scenario with 4 fixed tasks
 *
 * This is the header file defining the shared variables and commonly used
 * procedures.
 *
 */

#include <stdlib.h>
#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
#include <stdatomic.h>

const int BUF_SIZE = 5;
const char* CONTROL_PATH = "control";
const char* DATA_PATH = "data";

enum Mode
{
  stopped,
  running
}

Mode producer_control = stopped;
Mode consumer_control = stopped;

char ring_buffer[BUF_SIZE];


_Atomic(int) read_idx = 0;
int write_idx = 0;

// probably needs to be inlined ...
int buffer_next(int idx) {
  //return (idx+1)%BUF_SIZE;
  if (idx == BUF_SIZE-1)
    return 0;
  else
    return idx+1;
}
