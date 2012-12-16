main = interact respondPalindromes

respondPalindromes :: String -> String
respondPalindromes = unlines . map isPalindrome . lines

isPalindrome :: String -> String
isPalindrome x
    | x == reverse x = "Is a palindrome"
    | x /= reverse x = "Not a palindrome"
