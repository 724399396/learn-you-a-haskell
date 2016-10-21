main = interact respondPalindromes

respondPalindromes = unlines . map (\xs ->
    if isPalindrome xs then "palindrome" else "not a palindrrome") . lines
        where isPalindrome xs = xs == reverse xs