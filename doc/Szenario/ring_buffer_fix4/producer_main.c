/**
 * Version of ring_buffer scenario with 4 fixed tasks
 *
 * This is the producer task main routine. It reads data from a stream and
 * writes it to a shared ring buffer.
 *
 * Accesssed shared variables:
 *   producer_control    A (Mode) control flag for the producer task, set by the
 *                       controler to suspend and resume the producer task
 *   ring_buffer         A (char*) pointer referring to the first element of an
 *                       array of character values (implementing the the ring
                        buffer)
 *   read_idx            The (int) index of the read had in the ring buffer array.
 *   write_idx           The (int) index of the write had in the ring buffer
 *                       array.
 */

#include "main.h"

void producer_main() {

  printf("Producer: startet.\n");

  //open data stream
  FILE* data_fp = fopen("data", "r");
  if(!data_fp) {
    perror("Data file opening failed");
    pthread_exit(NULL);
  }

  char input;

  while (!feof(data_fp) {
    if (producer_control == stopped) continue;

    printf("Producer startet.\n");

    while (producer_control==running) {
      if (buffer_next(write_idx) != read_idx) {
          ring_buffer[write_idx] = getc(data_fp);
          printf("Producer: Value %c written to buffer.\n", ring_buffer[write_idx]);
          write_idx = buffer_next(write_idx);
        }
    }

    printf("Producer: stopped.\n");
  }
  printf("Producer: terminated.\n");
}
