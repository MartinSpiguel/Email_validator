domains = ["gmail.com", "yahoo.com", "hotmail.com", "outlook.com", "icloud.com", 
           "aol.com", "mail.com", "yandex.com", "zoho.com", "protonmail.com", 
           "gmx.com", "live.com", "me.com", "inbox.com", "rocketmail.com"]

-- Validates the email
validator :: String -> Bool
validator mail | not (hasAt mail) || not (hasValidDomain mail domains)  || "@" ++ getDomain mail == mail = False
               | otherwise = True

-- Checks if mail has an at
hasAt :: String -> Bool
hasAt [] = False
hasAt (x:xs) = x == '@' || hasAt xs

-- Checks if mail has a known domain
hasValidDomain :: String -> [String] -> Bool
hasValidDomain _ [] = False
hasValidDomain mail (dom:doms) = getDomain mail == dom || hasValidDomain mail doms

-- Takes a mail and returns the domain
getDomain :: String -> String
getDomain (x:xs) | x /= '@' = getDomain xs
                 | otherwise = xs