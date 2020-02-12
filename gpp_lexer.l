%{
#include <stdio.h>
%}

%%
\,						printf("OP_COMMA\n");
\“						printf("OP_OC\n");
\”						printf("OP_CC\n");
\( 						printf("OP_OP\n");
\)						printf("OP_CP\n");
\+						printf("OP_PLUS\n");
\-						printf("OP_MINUS\n");
\/						printf("OP_DIV\n");
\*						printf("OP_MULT\n");
\**						printf("OP_DBLMULT\n");
[a][n][d]				printf("KW_AND\n");
[o][r]					printf("KW_OR\n");
[n][o][t]				printf("KW_NOT\n");
[e][q][u][a][l]			printf("KW_EQUAL\n");
[l][e][s][s]			printf("KW_LESS\n");
[d][e][f][f][u][n]		printf("KW_DEFFUN\n");
[f][o][r]				printf("KW_FOR\n");
[i][f]					printf("KW_IF\n");
[e][x][i][t]			printf("KW_EXIT\n");
[l][o][a][d]			printf("KW_LOAD\n");
[n][i][l]				printf("KW_NIL\n");
[l][i][s][t]			printf("KW_LIST\n");
[a][p][p][e][n][d]		printf("KW_APPEND\n");
[c][o][n][c][a][t]		printf("KW_CONCAT\n");
[s][e][t]				printf("KW_SET\n");
[d][i][s][p]		 	printf("KW_DISP\n");
[t][r][u][e]			printf("KW_TRUE\n");
[f][a][l][s][e]			printf("KW_FALSE\n");
[a-zA-Z][a-zA-Z0-9]*	printf("IDENTIFIER	\n");
[0]					    printf("VALUE\n");
[1-9][0-9]*				printf("VALUE\n");
[0][0-9]*				printf("INVALID_VALUE\n");
[;][;][a-zA-Z0-9.,-_*?\}=)({/&%$^#'!"+ ]*	printf("COMMENT\n");
[ \n]+					/* ignore newlines*/
[ \t]+					/* ignore whitespace */
%%

