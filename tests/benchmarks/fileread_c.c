#include <unicode/ustdio.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void lazystring(const char *name)
{
    FILE *ufp = fopen(name, "r");
    const size_t bufsize = sizeof(char) * 32 * 1024;
    char *str = malloc(bufsize);
    long len = 0;
    int32_t n;

    do {
	n = fread(str, sizeof(char), bufsize, ufp);
	len += n;
    } while (n > 0);

    printf("%ld\n", len);
}

void lazytext(const char *name)
{
    UFILE *ufp = u_fopen(name, "r", NULL, "UTF-8");
    const size_t bufsize = sizeof(UChar) * 32 * 1024;
    UChar *str = malloc(bufsize);
    long len = 0;
    int32_t n;

    do {
	n = u_file_read(str, bufsize, ufp);
	len += n;
    } while (n > 0);

    printf("%ld\n", len);
}

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

void string(const char *name)
{
    FILE *fp = fopen(name, "r");
    char *str;
    long fsize;
    int32_t n;

    fseek(fp, 0, SEEK_END);
    fsize = ftell(fp);
    fseek(fp, 0, SEEK_SET);

    str = malloc(sizeof(*str) * fsize);

    n = fread(str, sizeof(char), fsize, fp);

    printf("%d\n", n);
}

int main(int argc, char **argv)
{
    if (argc != 3) {
	fprintf(stderr, "Usage: %s handler filename\n", argv[0]);
	exit(1);
    }

    if (strcmp(argv[1], "lazystring") == 0)
	lazystring(argv[2]);
    else if (strcmp(argv[1], "lazytext") == 0)
	lazytext(argv[2]);
    else if (strcmp(argv[1], "string") == 0)
	string(argv[2]);
    else if (strcmp(argv[1], "text") == 0)
	text(argv[2]);
    else {
	fprintf(stderr, "no matching handler\n");
	return 1;
    }

    return 0;
}
