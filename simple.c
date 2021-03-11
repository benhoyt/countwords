#include <ctype.h>
#include <search.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_UNIQUES 60000

typedef struct {
    char* word;
    int count;
} count;

// Comparison function for qsort() ordering by count descending.
int cmp_count(const void* p1, const void* p2) {
    int c1 = ((count*)p1)->count;
    int c2 = ((count*)p2)->count;
    if (c1 == c2) return 0;
    if (c1 < c2) return 1;
    return -1;
}

int main() {
    // The hcreate hash table doesn't provide a way to iterate, so
    // store the words in an array too (also used for sorting).
    count* words = calloc(MAX_UNIQUES, sizeof(count));
    int num_words = 0;

    // Allocate hash table.
    if (hcreate(MAX_UNIQUES) == 0) {
        fprintf(stderr, "error creating hash table\n");
        return 1;
    }

    char word[101]; // 100-char word plus NUL byte
    while (scanf("%100s", word) != EOF) {
        // Convert word to lower case in place.
        for (char* p = word; *p; p++) {
            *p = tolower(*p);
        }

        // Search for word in hash table.
        ENTRY item = {word, NULL};
        ENTRY* found = hsearch(item, FIND);
        if (found != NULL) {
            // Word already in table, increment count.
            int* pn = (int*)found->data;
            (*pn)++;
        } else {
            // Word not in table, insert it with count 1.
            item.key = strdup(word); // need to copy word
            if (item.key == NULL) {
                fprintf(stderr, "out of memory in strdup\n");
                return 1;
            }
            int* pn = malloc(sizeof(int));
            if (pn == NULL) {
                fprintf(stderr, "out of memory in malloc\n");
                return 1;
            }
            *pn = 1;
            item.data = pn;
            ENTRY* entered = hsearch(item, ENTER);
            if (entered == NULL) {
                fprintf(stderr, "table full, increase MAX_UNIQUES\n");
                return 1;
            }

            // And add to words list for iterating.
            words[num_words].word = item.key;
            num_words++;
        }
    }

    // Iterate once to add counts to words list, then sort.
    for (int i = 0; i < num_words; i++) {
        ENTRY item = {words[i].word, NULL};
        ENTRY* found = hsearch(item, FIND);
        if (found == NULL) { // shouldn't happen
            fprintf(stderr, "key not found: %s\n", item.key);
            return 1;
        }
        words[i].count = *(int*)found->data;
    }
    qsort(&words[0], num_words, sizeof(count), cmp_count); 

    // Iterate again to print output.
    for (int i = 0; i < num_words; i++) {
        printf("%s %d\n", words[i].word, words[i].count);
    }

    return 0;
}
