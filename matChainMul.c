#include <stdlib.h>
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <limits.h>
//#include "../matMul/matMul.h" 

typedef struct Matrix {
	int nrows, ncols;
	int **rows;
	int index;
} Matrix;

typedef struct TreeNode {
	struct TreeNode *next, *left, *right;
	Matrix *matrix;
	int minCost;
	int range;
} TreeNode;

typedef struct Tree {
	TreeNode *head, *tail;
} Tree;

void printMatrix(Matrix *mat) {
	printf("Matrix ID %d, (%dX%d)\n", mat->index, mat->nrows, mat->ncols);
	for (unsigned int i = 0; i < mat->nrows; i++) {
		for (unsigned int k = 0; k < mat->ncols; k++) {
			printf("%d ", mat->rows[i][k]);
		}
		printf("\n");
	}
}

Matrix* multiplyMatrices(Matrix *left, Matrix *right) {
	if (left->ncols != right->nrows) {
		fprintf(stderr, "BAD left ncols %d right nrows %d\n", left->ncols, right->nrows);
		exit(EXIT_FAILURE);
	}
	printf("MULTIPLING %dx%dX%dx%d, index1 %d, index2 %d\n", left->nrows,
			left->ncols, right->nrows, right->ncols, left->index, right->index);
	int **rows = calloc(left->nrows, sizeof(int*));
	for (unsigned int i = 0; i < left->nrows; i++) {
		rows[i] = calloc(right->ncols, sizeof(int));
		for (unsigned int k = 0; k < right->ncols; k++) {
			int element = 0;
			for (unsigned int v = 0; v < left->ncols; v++) {
				element += left->rows[i][v] * right->rows[v][k];
			}
			rows[i][k] = element;
		}
	}
	for (unsigned int i = 0; i < left->nrows; i++) {
		free(left->rows[i]);
	}
	free(left->rows);
	left->rows = rows;
	left->ncols = right->ncols;
	printf("PROUCT is %dx%d\n", left->nrows, left->ncols);

	return left;
}

TreeNode* multiplyMatrixTree(TreeNode *tree) {
	if (tree == NULL)
		return NULL;

	if (tree->left != NULL && tree->left->range > 2)
		tree->left = multiplyMatrixTree(tree->left);

	if (tree->right != NULL && tree->right->range > 2)
		tree->right = multiplyMatrixTree(tree->right);

	if (tree->left != NULL) {
		if (tree->left->range == 2)
			tree->left->matrix = multiplyMatrices(tree->left->matrix, tree->left->matrix + 1);
		tree->left->range = 1;
	}

	if (tree->right != NULL) {
		if (tree->right->range == 2)
			tree->right->matrix = multiplyMatrices(tree->right->matrix, tree->right->matrix + 1);
		tree->right->range = 1;
	}

	if (tree->left != NULL && tree->right != NULL) {
		tree->left->matrix = multiplyMatrices(tree->left->matrix, tree->right->matrix);
		return tree->left;
	} else if (tree->left != NULL)
		return tree->left;
	return tree->right;
}

TreeNode* getNextNode(Tree *tree) {
	if (tree->head == NULL) {
		tree->head = tree->tail = calloc(1, sizeof(TreeNode));
	} else {
		tree->tail->next = calloc(1, sizeof(TreeNode));
		tree->tail = tree->tail->next;
	}

	return tree->tail;
}

void resetTree(Tree *tree, TreeNode *startPartition, TreeNode *endPartition) {
	TreeNode *node = startPartition->next;
	if (startPartition == endPartition)
		return;
	while (node != endPartition) {
		TreeNode *tmp = node->next;
		free(node);
		node = tmp;
	}

	if (endPartition != NULL) {
		tree->tail = endPartition;
		startPartition->next = endPartition;
	} else {
		tree->tail = startPartition;
		startPartition->next = NULL;
	}
}

/* Loop through all possibilities to find the smallest cost partiton */
TreeNode* findLowestCostPartition(Matrix *mats, Tree *tree, int startRange, int endRange) {
	int range = endRange - startRange + 1;

	if (range < 3) {
		TreeNode *node = getNextNode(tree);
		node->matrix = &mats[startRange];
		node->range = range;
		if (range == 1) {
			node->minCost = 0;
		} else {
			node->minCost = mats[startRange].nrows * mats[startRange].ncols
					* mats[endRange].ncols;
		}
		return node;
	}
	int k, mink, minCost = INT_MAX, cost;
	TreeNode *result = getNextNode(tree);
	TreeNode *sleft, *sright;
	TreeNode *startPartition = tree->tail;

	for (k = startRange; k < endRange; k++) {
		/* keep track of the partition start and end, so we can replace it with a smaller cost partition if found later */
		TreeNode *endPartition = tree->tail;
		TreeNode *left = findLowestCostPartition(mats, tree, startRange, k);
		TreeNode *right = findLowestCostPartition(mats, tree, k + 1, endRange);

		int count1 = 0;
		int count2 = 0;
		int count3 = 0;
		if (left != NULL) {
			count1 = left->minCost;
		}
		if (right != NULL) {
			count2 = right->minCost;
		}
		count3 = mats[startRange].nrows * mats[k].ncols * mats[endRange].ncols;
		cost = count1 + count2 + count3;

		if (cost >= minCost) {
			if (endPartition != NULL)
				resetTree(tree, endPartition, NULL);
			else resetTree(tree, startPartition, NULL);
		} else {
			resetTree(tree, startPartition, endPartition);
			minCost = cost;
			mink = k;
			sleft = left;
			sright = right;
		}
	}

	result->matrix = &mats[mink];
	result->range = range;
	result->left = sleft;
	result->right = sright;
	result->minCost = minCost;

	return result;
}

Matrix* readMatrixDetails(char *filename, int *count) {
	Matrix *mats = NULL;
	int matrixCount;
	FILE *fp = fopen(filename, "r");

	if (!fp) {
		perror("fopen failed");
		exit(EXIT_FAILURE);
	}

	if (!fscanf(fp, "%d\n", &matrixCount)) {
		perror("reading the number of matrices failed");
		exit(EXIT_FAILURE);
	}
	*count = matrixCount;
	mats = calloc(matrixCount, sizeof(Matrix));

	for (unsigned int matIndex = 0; matIndex < matrixCount; matIndex++) {
		Matrix *mat = &mats[matIndex];

		if (!fscanf(fp, "%d %d\n", &mat->nrows, &mat->ncols)) {
			perror("reading the dimensions of matrix failed");
			exit(EXIT_FAILURE);
		}
		mat->index = matIndex;
		mat->rows = calloc(mat->nrows, sizeof(int*));
		for (unsigned int i = 0; i < mat->nrows; i++) {
			mat->rows[i] = calloc(mat->ncols, sizeof(int));
			for (unsigned int k = 0; k < mat->ncols; k++) {
				int element;
				if (!fscanf(fp, "%d ", &element)) {
					perror("reading the element of matrix failed");
					exit(EXIT_FAILURE);
				}
				mat->rows[i][k] = element;
			}
		}
		printMatrix(mat);
	}
	fclose(fp);
	return mats;
}

void freeMatrixData(Matrix *mats, Tree *tree, int matrixCount) {
	for (unsigned int matIndex = 0; matIndex < matrixCount; matIndex++) {
		Matrix *mat = &mats[matIndex];

		for (unsigned int i = 0; i < mat->nrows; i++) {
			free(mat->rows[i]);
		}
		free(mat->rows);
	}
	free(mats);
	TreeNode *node = tree->head;
	while (node != NULL) {
		TreeNode *tmp = node->next;
		free(node);
		node = tmp;
	}
	free(tree);
}

int main(int argc, char *argv[]) {
	int matrixCount;
	Matrix *mats = readMatrixDetails(argv[1], &matrixCount);
	Tree *tree = calloc(1, sizeof(Tree));
	TreeNode *treenode = findLowestCostPartition(mats, tree, 0, matrixCount - 1);
	printf("Final cost %d\n", treenode->minCost);
	multiplyMatrixTree(treenode);
	freeMatrixData(mats, tree, matrixCount);

	exit(EXIT_SUCCESS);
}

