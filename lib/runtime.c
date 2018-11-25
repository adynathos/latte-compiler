#include <stdlib.h>
#include <stdio.h>
#include <string.h>

// string operations
char strBuffer[2048];

char* _internal_return_string_from_buffer(char* buffer) {
	char* retStr;
	int len = strlen(buffer);
	retStr = malloc(sizeof(char)*(len+1));
	strcpy(retStr, buffer);
	return retStr;
}

char* _internal_string_add(char* a, char* b) {
	strBuffer[0] = '\0';
    strcat(strBuffer, a);
    strcat(strBuffer, b);
	return _internal_return_string_from_buffer(strBuffer);
}

/*
	case LangRel_EQ() => 0
	case LangRel_NE() => 1
	case LangRel_GE() => 2
	case LangRel_GT() => 3
	case LangRel_LE() => 4
	case LangRel_LT() => 5
*/
int _internal_string_compare(char* a, char* b, int op) {
	int cmp = strcmp(a, b);

	switch(op) {
		case 0:
			return cmp == 0;
		case 1:
			return cmp != 0;
		case 2:
			return cmp >= 0;
		case 3:
			return cmp == 1;
		case 4:
			return cmp <= 0;
		case 5:
			return cmp == -1;
		default:
			return 0;
	}
}

// IO functions
void printInt(int int_val) {
	printf("%d\n", int_val);
}

void printString(char* str) {
	printf("%s\n", str);
}

int readInt() {
	int int_val;
	scanf("%d", &int_val);
	return int_val;
}


char* readString() {
	scanf("%s", strBuffer);
	return _internal_return_string_from_buffer(strBuffer);
}

void error() {
	printf("runtime error\n");
	exit(1);
}

