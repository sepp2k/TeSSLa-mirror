/**
 * Version of ring_buffer scenario with 4 fixed tasks
 *
 * This is the consumer task 1 main routine. It reads and processes data from a
 * shared ring buffer.
 *
 * Accesssed shared variables:
 *   consumer_control    A (Mode) control flag for the producer task, set by the
 *                       controler to suspend and resume the producer task
 *   ring_buffer         A (char*) pointer referring to the first element of an
 *                       array of character values (implementing the the ring
                        buffer)
 *   read_idx            The (int) index of the read had in the ring buffer array.
 *   write_idx           The (int) index of the write had in the ring buffer
 *                       array.
 */

#include "main.h"

void consumer_main() {
  // Change this ID for further consumer instances
  int id = 1;

  char data = 0;
  int local_read_idx = 0;
  int next_read_idx = 0;

  while {
    if (consumer_control == stopped) continue;

    printf("Consumer %ld: startet.\n", id);

    while (consumer_control == running || read_idx!=write_idx) {
      local_read_idx = read_idx;
      if (local_read_idx != write_idx) {
        data = ring_buffer[local_read_idx];
        next_read_idx = buffer_next(local_read_idx);
        if (atomic_compare_exchange_weak(&read_idx, &local_read_idx, next_read_idx)) {
          //process data
          process_data(&data)
          printf("Consumer %ld: processed data %c\n", id, data);
        }
      }
    }

    printf("Consumer %ld: stopped.\n", id);
  }
}
