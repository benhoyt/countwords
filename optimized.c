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

typedef struct {
    void *prev;
    char buf[BUF_SIZE];
} wbuf;

wbuf* cur_wbuf;
int wbuf_pos = 0;

// Used both for hash table buckets and array for sorting.
typedef struct {
    char* word;
    int word_len;
    int count;
} count;

// Comparison function for qsort() ordering by count descending.
static int cmp_count(const void* p1, const void* p2) {
    int c1 = ((count*)p1)->count;
    int c2 = ((count*)p2)->count;
    if (c1 == c2) return 0;
    if (c1 < c2) return 1;
    return -1;
}

count* table;
int num_unique = 0;

// Increment count of word in hash table (or insert new word).
static void increment(char* word, int word_len, uint64_t hash) {
    // Make 64-bit hash in range for items slice.
    int index = (int)(hash & (uint64_t)(HASH_LEN-1));

    // Look up key, using direct match and linear probing if not found.
    while (1) {
        if (table[index].word == NULL) {
            if (wbuf_pos + word_len >= BUF_SIZE) {
                wbuf *new_wbuf = malloc(sizeof(wbuf));

                if (new_wbuf == NULL) {
                    fprintf(stderr, "out of memory\n");
                    exit(1);
                }
                new_wbuf->prev = cur_wbuf;
                cur_wbuf = new_wbuf;
                wbuf_pos = 0;
            }
            char *word_copy = &cur_wbuf->buf[wbuf_pos];
            wbuf_pos += word_len;

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

    cur_wbuf = malloc(sizeof(wbuf));
    if (cur_wbuf == NULL) {
        fprintf(stderr, "out of memory\n");
        return 1;
    }
    cur_wbuf->prev = NULL;

    char buf[BUF_SIZE + 3];
    char *buf_end = buf + fread(buf, 1, BUF_SIZE , stdin);

    while (1) {
        // extra word + space to avoid checks for size 
        buf_end[0] = ' ';
        buf_end[1] = 'a';
        buf_end[2] = ' ';

        char *word_start = buf;
        char *word_end = buf;

        while (1) {
            uint64_t word_hash = FNV_OFFSET;

            // Skip whitespace to find next word_start
            while (*word_start <= ' ') ++word_start;
            word_end = word_start;

            // Read till we find word_end and transform / compute hash on the way
            while (*word_end > ' ') {
                if (*word_end >= 'A' && *word_end <= 'Z')
                    *word_end += ('a' - 'A');
                word_hash *= FNV_PRIME;
                word_hash ^= *word_end;
                ++word_end;
            }

            // If the word end is beyond buf_end
            if (word_end >= buf_end) break;

            increment(word_start, word_end - word_start, word_hash);
            word_start = ++word_end;
        }

        size_t remaining = word_start < buf_end? buf_end - word_start : 0;

        memmove(buf, word_start, remaining);
        buf_end = buf + remaining;
        buf_end += fread(buf_end, 1, BUF_SIZE - remaining, stdin);

        // End of input
        if (buf_end == buf + remaining) break;
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

    free(table);
    while(cur_wbuf != NULL) {
        wbuf *prev_wbuf = (wbuf *)cur_wbuf->prev;

        free(cur_wbuf);
        cur_wbuf = prev_wbuf;
    }

    return 0;
}
