#include <ctype.h>
#include <search.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define BUF_SIZE 65536
#define HASH_LEN 65536  // must be a power of 2
#define FNV_OFFSET 14695981039346656037UL
#define FNV_PRIME 1099511628211UL

// Used both for hash table buckets and array for sorting.
typedef struct {
    char* word;
    int word_len;
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

count* table;
int num_unique = 0;

// Increment count of word in hash table (or insert new word).
void increment(char* word, int word_len, uint64_t hash) {
    // Make 64-bit hash in range for items slice.
    int index = (int)(hash & (uint64_t)(HASH_LEN-1));

    // Look up key, using direct match and linear probing if not found.
    while (1) {
        if (table[index].word == NULL) {
            // Found empty slot, add new item (copying key).
            char* word_copy = malloc(word_len);
            if (word_copy == NULL) {
                fprintf(stderr, "out of memory\n");
                exit(1);
            }
            memmove(word_copy, word, word_len);
            table[index].word = word_copy;
            table[index].word_len = word_len;
            table[index].count = 1;
            num_unique++;
            return;
        }
        if (table[index].word_len == word_len &&
                memcmp(table[index].word, word, word_len) == 0) {
            // Found matching slot, increment existing count.
            table[index].count++;
            return;
        }
        // Slot already holds another key, try next slot (linear probe).
        index++;
        if (index >= HASH_LEN) {
            index = 0;
        }
    }
}

int main() {
    // Allocate hash table buckets.
    table = calloc(HASH_LEN, sizeof(count));
    if (table == NULL) {
        fprintf(stderr, "out of memory\n");
        return 1;
    }

    char buf[BUF_SIZE];
    int offset = 0;
    while (1) {
        // Read file in chunks, processing one chunk at a time.
        size_t num_read = fread(buf+offset, 1, BUF_SIZE-offset, stdin);
        if (num_read+offset == 0) {
            break;
        }

        // Find last space or linefeed in buf and process up to there.
        int space;
        for (space = offset+num_read-1; space>=0; space--) {
            char c = buf[space];
            if (c <= ' ') {
                break;
            }
        }
        int num_process = (space >= 0) ? space : (int)num_read+offset;

        // Scan chars to process: tokenize, lowercase, and hash as we go.
        int i = 0;
        while (1) {
            // Skip whitespace before word.
            for (; i < num_process; i++) {
                char c = buf[i];
                if (c > ' ') {
                    break;
                }
            }
            // Look for end of word, lowercase and hash as we go.
            uint64_t hash = FNV_OFFSET;
            int start = i;
            for (; i < num_process; i++) {
                char c = buf[i];
                if (c <= ' ') {
                    break;
                }
                if (c >= 'A' && c <= 'Z') {
                    c += ('a' - 'A');
                    buf[i] = c;
                }
                hash *= FNV_PRIME;
                hash ^= (uint64_t)c;
            }
            if (i <= start) {
                break;
            }
            // Got a word, increment count in hash table.
            increment(buf+start, i-start, hash);
        }

        // Move down remaining partial word.
        if (space >= 0) {
            offset = (offset+num_read-1) - space;
            memmove(buf, buf+space+1, offset);
        } else {
            offset = 0;
        }
    }

    count* ordered = calloc(num_unique, sizeof(count));
    for (int i=0, i_unique=0; i<HASH_LEN; i++) {
        if (table[i].word != NULL) {
            ordered[i_unique++] = table[i];
        }
    }
    qsort(ordered, num_unique, sizeof(count), cmp_count);
    for (int i=0; i<num_unique; i++) {
        printf("%.*s %d\n",
                ordered[i].word_len, ordered[i].word, ordered[i].count);
    }

    return 0;
}
