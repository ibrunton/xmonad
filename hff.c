/*
 *        Project:  hff
 *       Filename:  main.c
 *
 *    Description:  read integers from file, return wrapped in color codes
 *    				specific to xmobar
 *
 *        Version:  1.0
 *        Created:  2013-4-14 11:18
 *       Compiler:  gcc -o hff hff-c
 *
 *         Author:  Ian D Brunton (ib), iandbrunton at gmail .com
 *
 */

#include <stdio.h>
#include <stdlib.h>

int
main (int argc, char *argv[])
{
	int value = 0;
	/* default colours */
	char *colour1 = "e0e0e0";
	char *colour2 = "09ba55";
   	char *colour3 = "cd5666";
	char *endcolour = "</fc>";
	FILE *file;

	if (argc < 2)
		return EXIT_FAILURE;

	if (argc > 2)
		colour1 = argv[2];
	if (argc > 3)
		colour2 = argv[3];
	if (argc > 4)
		colour3 = argv[4];

	file = fopen (argv[1], "r");
	fscanf (file, "%d", &value);
	fclose (file);

	if (value < 0) {
		printf ("<fc=#%s>%d%s", colour3, value, endcolour);
	}
	else if (value > 0) {
		printf ("<fc=#%s>%d%s", colour2, value, endcolour);
	}
	else {
		printf ("<fc=#%s>%d%s", colour1, value, endcolour);
	}

	return EXIT_SUCCESS;
}
