/**
 * Version of ring_buffer scenario with 4 fixed tasks
 *
 * This is the controller task main routine. It reads control commands from a
 * stream and suspends or resumes the producer and consumer tasks.
 *
 * Accesssed shared variables:
 *   producer_control    An (int) control flag for the producer task, set by the
 *                       controler to suspend and resume the producer task
 *   consumer_control    An (int) control flag for the producer task, set by the
 *                       controler to suspend and resume the producer task
 */

#include "main.h"

void controller_main() {

  //open control stream
  FILE* ctrl_fp = fopen("control", "r");
  if(!ctrl_fp) {
    perror("File opening failed");
    return EXIT_FAILURE;
  }

  char input;
  Mode mode = stopped;

  while ((input = getc(ctrl_fp)) != EOF) {
    printf("Control: input: %c\n", input);
    if (input == 'r' && mode == stopped) {
      mode = running;
      producer_control = running;
      consumer_control = running;
      printf("Control: operation mode.\n");
    } else if (input == 's' && mode == running) {
      printf("Control: stopping tasks.\n");

      mode = stopped;
      producer_control = stopped;
      consumer_control = stopped;

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
  if (mode == running) {
    producer_control = stopped;
    consumer_control = stopped;
  }

}
