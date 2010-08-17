#include <unicode/ustdio.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void text(const char *name)
{
    UFILE *ufp = u_fopen(name, "r", NULL, "UTF-8");
    FILE *fp = u_fgetfile(ufp);
    UChar *str;
    long fsize;
    int32_t n;

    fseek(fp, 0, SEEK_END);
    fsize = ftell(fp);
    u_frewind(ufp);

    str = malloc(sizeof(*str) * fsize);

    n = u_file_read(str, fsize, ufp);

    printf("%d\n", n);
}

int main(int argc, char **argv)
{
    if (argc != 3) {
	fprintf(stderr, "Usage: fileread handler filename\n");
	exit(1);
    }

    if (strcmp(argv[1], "text") == 0)
	text(argv[2]);
    else {
	fprintf(stderr, "no matching handler\n");
	return 1;
    }

    return 0;
}
