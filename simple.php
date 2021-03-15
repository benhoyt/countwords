<?php

$words = [];
while (($line = fgets(STDIN)) !== false) {
    foreach (preg_split('/ +/', strtolower(trim($line))) as $word) {
        if (!isset($words[$word])) {
            $words[$word] = 0;
        }
        $words[$word]++;
    }
}

arsort($words);

foreach($words as $word => $count) {
    echo "$word $count\n";
}
