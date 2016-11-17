compile, e.g., with 
> gcc -std=gnu11 -pthread main.c

make input and output files, i.g., use named pipes for interaction:
> mkfifo data
> mkfifo control

run program
> ./a.out

pipe commands ('o' for operation, 'm' for maintenance) into "control" pipe:
>  echo "o" > control

pipe data into "data" pipe
> echo "abcdefg" > data

Have fun.