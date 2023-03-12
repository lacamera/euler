```sh
clang -lm -std=c2x -o p[..n] p[..n].c
```

```sh
clang -lm -std=c2x -g -o p1 p1.c
objdump -Sl p1 | less
```
