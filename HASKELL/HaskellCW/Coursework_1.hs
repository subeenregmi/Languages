import qualified Data.List as L
import qualified Data.Maybe as M
import qualified Data.Map as DMap
------------------------- Merge sort

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
    | x <  y    = x : merge    xs (y:ys)
    | x == y    = x : merge    xs    ys
    | otherwise = y : merge (x:xs)   ys

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = msort (take n xs) `merge` msort (drop n xs)
  where
    n = length xs `div` 2
    
------------------------- Game world types

type Character = String
type Party     = [Character]

type Node      = Int
type Location  = String
type Map       = [(Node,Node)]

data Game      = Over
               | Game Map Node Party [Party]
  deriving (Eq,Show)

type Event     = Game -> Game


testGame :: Node -> Game
testGame i = Game [(0,1)] i ["Russell"] [[],["Brouwer","Heyting"]]

------------------------- Assignment 1: The game world

connected :: Map -> Node -> [Node]
connected [] _ = []
connected (y:ys) x
    | first == x  = second : connected ys x
    | second == x = first : connected ys x
    | otherwise   = connected ys x
  where 
    first  = fst y
    second = snd y

connect :: Node -> Node -> Map -> Map
connect x y [] = if x < y 
                    then [(x, y)] 
                    else [(y, x)]
connect x y (z:zs)
    | x == y                        = error "Cannot connect same nodes together"
    | (x, y) == z || (y, x) == z    = z : zs
    | x < y && greaterThan z (x, y) = (x, y) : z : zs              
    | x > y && greaterThan z (y, x) = (y, x) : z : zs              
    | otherwise                     = z : connect x y zs
  where
    greaterThan :: (Node, Node) -> (Node, Node) -> Bool
    greaterThan (z, f) (x, y)
      | z > x           = True
      | z == x && f > y = True
      | otherwise       = False

disconnect :: Node -> Node -> Map -> Map
disconnect x y [] = []
disconnect x y (z:zs)
    | x < y && ((x, y) == z) = zs
    | x > y && ((y, x) == z) = zs
    | otherwise              = z : disconnect x y zs

add :: Party -> Event
add _ Over                 = Over
add [] g                   = g
add (x:xs) (Game m n p ps) = add xs $ Game m n (merge [x] p) ps

addAt :: Node -> Party -> Event
addAt _ _ Over                   = Over
addAt _ [] g                     = g
addAt node party (Game m n p ps) = Game m n p (newPs ps node party) 
  where
    newPs :: [Party] -> Node -> Party -> [Party]
    newPs [] node party = (take (node) $ repeat([])) ++ [party] 
    -- the above is for when specifying when n is a location which you haven't been to
    -- thus the brackets in ps haven't been generated
    newPs (x:xs) node party
        | node == 0 = (merge party x) : xs
        | otherwise = x : newPs xs (node-1) party

addHere :: Party -> Event
addHere party (Game m n p ps) = addAt n party (Game m n p ps)

remove :: Party -> Event
remove _ Over                 = Over
remove [] g                   = g
remove (x:xs) (Game m n p ps) = remove xs (Game m n (L.delete x p) ps)

removeAt :: Node -> Party -> Event
removeAt _ _ Over = Over
removeAt _ [] g = g
removeAt node party (Game m n p ps) = Game m n p (newPs ps node party)
  where
    deleteCharacter []     y = y
    deleteCharacter (x:xs) y = deleteCharacter xs (L.delete x y)

-- TODO: comment and clear up code
    newPs [] node party = []
    newPs (x:xs) node party
        | node == 0 = (deleteCharacter party x) : xs
        | otherwise = x : newPs xs (node-1) party

removeHere :: Party -> Event
removeHere party (Game m n p ps) = removeAt n party (Game m n p ps)

------------------------- Assignment 2: Dialogues


data Dialogue = Action  String  Event
              | Branch  (Game -> Bool) Dialogue Dialogue
              | Choice  String  [( String , Dialogue )]

testDialogue :: Dialogue
testDialogue = Branch ( isAtZero )
  (Choice "Russell: Let's get our team together and head to Error." [])
  (Choice "Brouwer: How can I help you?"
    [ ("Could I get a haircut?", Choice "Brouwer: Of course." [])
    , ("Could I get a pint?",    Choice "Brouwer: Of course. Which would you like?"
      [ ("The Segmalt.",     Action "" id)
      , ("The Null Pinter.", Action "" id)]
      )
    , ("Will you join us on a dangerous adventure?", Action "Brouwer: Of course." (add ["Brouwer"] . removeHere ["Brouwer"]))
    ]
  )
 where
  isAtZero (Game _ n _ _) = n == 0

dialogue :: Game -> Dialogue -> IO Game

dialogue g (Action s e) = do putStrLn s
                             return $ e g

dialogue g (Branch condition d1 d2)
    | condition g = dialogue g d1
    | otherwise   = dialogue g d2

dialogue g (Choice s []) =
  do putStrLn s
     return g

dialogue g (Choice s sd) = 
  do putStrLn s
     putStr(genChoices 1 sd)
     userChoice <- getLine
     if userChoice == "0" --(4a)
        then return g 
        else do let x = reads userChoice :: [(Int, String)] -- (4c)
                if rangeCheck x
                   then dialogue g (getChoiceFromIndex x)
                   else dialogue g (Choice s sd) 
  where
     genChoices _ []     = (">> ")
     genChoices i (x:xs) = "  " ++ show i ++ ". " ++ fst x ++ "\n" ++ genChoices (i+1) xs

     rangeCheck x = if null x
                       then False
                       else (1 <= (fst $ x !! 0)) && ((fst $ x !! 0) <= length sd)

     getChoiceFromIndex x = snd (sd !! ((fst $ x !! 0) - 1))

findDialogue :: Party -> Dialogue
findDialogue p = findDiaIn p theDialogues 
  where
    findDiaIn _ [] = Choice "There is nothing we can do." []
    findDiaIn p (x:xs)
        | fst x == msort p = snd x
        | otherwise        = findDiaIn p xs



------------------------- Assignment 3: The game loop

step :: Game -> IO Game
step Over = return Over
step g@(Game m n p ps) = 
  do let (menuStr, locations, people) = displayMenu g
     putStr (menuStr)
     choice <- getLine

     let choiceList = turnInputToChoiceList choice -- (4c)
         choiceLength = length choiceList
         validChoices = isAllNumbers choiceList

     if choiceList == ["0"] -- (4b)
        then step Over
        else if choiceLength == 0 || not validChoices
                then return g
                else let convertedChoices = map (read) choiceList :: [Int]
                         d = handleChoice convertedChoices (locations, people)
                     in dialogue g d

  where
    -- This function removes all spaces in a string and the remaining string is
    -- split up into a list of strings that contain a single character.
    turnInputToChoiceList x = filter (\x -> x /= " ") $ L.groupBy (\x y -> x /= ' ' && y /= ' ') x

    changeLocation :: Node -> Event
    changeLocation newN (Game m n p ps) = Game m newN p ps

    handleChoice choices (l, p)
        | length choices == 1 && locationRangeCheck = Action "" (changeLocation (locationToIndex firstChoice))
        | partyRangeCheck                           = findDialogue $ map (indexToName) (choices)
        | otherwise                                 = Choice "" [] 
      where
        firstChoice = choices !! 0
        -- This check checks if the choice is to change the location works as the locations
        -- start to index from 1 and the people are indexed after.
        locationRangeCheck = (firstChoice >= 1) && (firstChoice <= length l)
        partyRangeCheck = L.all (\x -> (x > length l) && (x <= length l + length p)) choices
        -- We find the character the user has chosen and finding the index in the list
        -- 'theLocations; need to subtract 1 as list indexing starts from 1
        locationToIndex i = M.fromJust $ L.elemIndex (l !! (i-1)) theLocations
        indexToName i = p !! (i - 1 - length l)

    isAllNumbers [] = True
    isAllNumbers (x:xs)
        | null evaluated                   = False
        | not.null $ snd $ evaluated !! 0  = False
        | otherwise                        = True && isAllNumbers xs
      where
        evaluated = reads x :: [(Int, String)]

    getTravelLocations :: Node -> Map -> [Location]
    getTravelLocations _ [] = []
    getTravelLocations n ((x,y):xs) = 
      if x == n || y == n
         -- use List difference here as we do not want to be able to travel to the a location
         -- where the user is already in
         then merge [theLocations !! x, theLocations !! y] (getTravelLocations n xs) L.\\ [theLocations !! n]
         else getTravelLocations n xs

    getPartyAtNode n ps
        | n >= length ps = []
        | otherwise      = ps !! n

    indexList _ [] = ""
    indexList i (x:xs) = " " ++ show i ++ ". " ++ x ++ "\n" ++ indexList (i+1) (xs)

    displayMenu (Game m n p ps) =
      let currentLocation = "You are in " ++ (theDescriptions !! n) ++ "\n"          

          locations = getTravelLocations n m
          locationPhrase = if null locations
                               then ""
                               else "You can travel to:\n" ++ indexList 1 locations

          partyPhrase = if null p
                           then ""
                           else "With you are:\n" ++ indexList (1+ length locations) p

          seeable = getPartyAtNode n ps
          seeablePhrase = if null seeable 
                             then ""
                             else "You can see:\n" ++ indexList (1+length locations + length p) seeable
          endPhrase = "What will you do?\n>> "
      in (currentLocation ++ locationPhrase ++ partyPhrase ++ seeablePhrase ++ endPhrase, locations, p++seeable)

game :: IO ()
game = do loop start
          return ()
       where
          loop g = do x <- step g
                      y <- step Over
                      if x == y
                         then return ()
                         else loop x  
------------------------- Assignment 4: Safety upgrades
{- 4a) Line 162
 - 4b) Line 197
 - 4c) Line 193, 160
 -}


------------------------- Assignment 5: Solving the game

data Command  = Travel [Int] | Select Party | Talk [Int]
  deriving Show

type Solution = [Command]

talk :: Game -> Dialogue -> [(Game,[Int])]
talk Over _ = []
-- if talk is called on branch then we can go through it without need of traversing
talk g (Branch condition d1 d2) = if condition g
                                     then talk g d1
                                     else talk g d2
talk g (Action _ _) = []
talk g (Choice _ []) = []
-- If it called on action or an empty choice, (which probably shouldnt be the case), we return nothing

talk g (Choice _ p) = traverse g (getList p) (Choice "" p) []
  where
    getList x = take (length x) [1..]
    getIndex elem list = (M.fromJust $ L.elemIndex (elem) (list)) + 1

    -- Essentially traverse continues down a path until it reaches either a empty choice, when
    -- this happens we just return [], when it reaches an action we return the correct data in 
    -- a list, after disovering all paths, we add all these results giving us all possible actions
    -- and the steps needed to take.
    traverse g (x:xs) (Action _ e) steps       = [(e g, steps)]

    traverse g (x:xs) (Branch con d1 d2) steps = if con g 
                                                    then traverse g (xs) d1 (steps ++ [x])
                                                    else traverse g (xs) d2 (steps ++ [x])

    traverse g _ (Choice _ []) steps           = []
    traverse g [] _ _ = []
    traverse g (x:xs) (Choice _ (p:ps)) steps  = 
        traverse g (getList (p:ps)) (snd p) (steps ++ [x]) ++ traverse g xs (Choice "" ps) steps

select :: Game -> [Party]
select (Game m n p ps) = L.subsequences (p ++ (ps !! n))

travel :: Map -> Node -> [(Node,[Int])]
travel m n = let mapGraph = createGraph m
                 allPossibleRoutes = traverse n n (getConnections n mapGraph) mapGraph [(n, [])] []
             in DMap.toList $ createShortestRoutes (allPossibleRoutes) DMap.empty 
  where

    createShortestRoutes [] s          = s
    createShortestRoutes ((y,ys):xs) s = if DMap.member y s
                                            then if (length ys) < length (s DMap.! y)
                                                    then createShortestRoutes xs (DMap.insert y ys s)
                                                    else createShortestRoutes xs s
                                            else createShortestRoutes xs (DMap.insert y ys s)
    getConnections node graph = graph DMap.! node

    -- d is a list of discoveredRoutes for nodes, mG a graph of the map 
    -- steps track the input passed in to get to the currentNode,  
    -- this works by going to the starting node, looking at its first connection, if it hasn't
    -- been discovered previously, then go to it, if it has go to the next connection, until all
    -- nodes have been discovered then return the possible routes.
    traverse startingNode currentNode [] mG d steps = d
    traverse startingNode currentNode (con:cons) mG d steps
        | elem (con) (discoveredNodes) = traverse startingNode currentNode cons mG d steps
        | otherwise                    = traverse startingNode con (getConnections con mG) mG (newD) (newSteps) ++ traverse startingNode currentNode cons mG d steps
      where
        discoveredNodes = map (fst) d
        index           = (M.fromJust $ L.elemIndex (con) (getConnections currentNode mG))+1
        newSteps        = steps ++ [index]
        newD            = d ++ [(con, newSteps)]


-- This map is called a graph, its keys are the nodes in a map and the values are the nodes
-- it is connected to.
createGraph []        = DMap.empty
createGraph (n:ns)    = addToGraph n (createGraph ns)
addToGraph (n1, n2) p = DMap.insertWith (merge) n2 [n1] $ DMap.insertWith (merge) n1 [n2] p

allSteps :: Game -> [(Solution,Game)]
allSteps (Game m n p ps) = 

solve :: Game -> Solution
solve = undefined

walkthrough :: IO ()
walkthrough = (putStrLn . unlines . filter (not . null) . map format . solve) start
  where
    format (Travel []) = ""
    format (Travel xs) = "Travel: " ++ unwords (map show xs)
    format (Select xs) = "Select: " ++ foldr1 (\x y -> x ++ ", " ++ y) xs
    format (Talk   []) = ""
    format (Talk   xs) = "Talk:   " ++ unwords (map show xs)

------------------------- Game data

start :: Game
start = Game theMap 0 [] theCharacters

theMap :: Map
theMap = [(1,2),(1,6),(2,4)]

theLocations :: [Location]
theLocations =
  -- Logicester
  [ "Home"           -- 0
  , "Brewpub"        -- 1
  , "Hotel"          -- 2
  , "Hotel room n+1" -- 3
  , "Temple"         -- 4
  , "Back of temple" -- 5
  , "Takeaway"       -- 6
  , "The I-50"       -- 7
  ]

theDescriptions :: [String]
theDescriptions =
  [ "your own home. It is very cosy."
  , "the `Non Tertium Non Datur' Brewpub & Barber's."
  , "the famous Logicester Hilbert Hotel & Resort."
  , "front of Room n+1 in the Hilbert Hotel & Resort. You knock."
  , "the Temple of Linearity, Logicester's most famous landmark, designed by Le Computier."
  , "the back yard of the temple. You see nothing but a giant pile of waste paper."
  , "Curry's Indian Takeaway, on the outskirts of Logicester."
  , "a car on the I-50 between Logicester and Computerborough. The road is blocked by a large, threatening mob."
  ]

theCharacters :: [Party]
theCharacters =
  [ ["Bertrand Russell"]                    -- 0  Home
  , ["Arend Heyting","Luitzen Brouwer"]     -- 1  Brewpub
  , ["David Hilbert"]                       -- 2  Hotel
  , ["William Howard"]                      -- 3  Hotel room n+1
  , ["Jean-Yves Girard"]                    -- 4  Temple
  , []                                      -- 5  Back of temple
  , ["Haskell Curry", "Jean-Louis Krivine"] -- 6  Curry's takeaway
  , ["Gottlob Frege"]                       -- 7  I-50
  ]

theDialogues :: [(Party,Dialogue)]
theDialogues = let
  always _ = True
  end str  = Choice str []
  isconn  _ _  Over           = False
  isconn  i j (Game m _ _ _ ) = elem i (connected m j)
  here         Over           = 0
  here        (Game _ n _ _ ) = n
  inParty   _  Over           = False
  inParty   c (Game _ _ p _ ) = elem c p
  isAt    _ _  Over           = False
  isAt    n c (Game _ _ _ ps) = elem c (ps !! n)
  updateMap _  Over           = Over
  updateMap f (Game m n p ps) = Game (f m) n p ps
 in
  [ ( ["Russell"] , Choice "Russell: Let's go on an adventure!"
      [ ("Sure." , end "You pack your bags and go with Russell.")
      , ("Maybe later.", end "Russell looks disappointed.")
      ]
    )
  , ( ["Heyting","Russell"] , end "Heyting: Hi Russell, what are you drinking?\nRussell: The strong stuff, as usual." )
  , ( ["Bertrand Russell"] , Branch (isAt 0 "Bertrand Russell") ( let
      intro = "A tall, slender, robed character approaches your home. When he gets closer, you recognise him as Bertrand Russell, an old friend you haven't seen in ages. You invite him in.\n\nRussell: I am here with a important message. The future of Excluded-Middle Earth hangs in the balance. The dark forces of the Imperator are stirring, and this time, they might not be contained.\n\nDo you recall the artefact you recovered in your quest in the forsaken land of Error? The Loop, the One Loop, the Loop of Power? It must be destroyed. I need you to bring together a team of our finest Logicians, to travel deep into Error and cast the Loop into lake Bottom. It is the only way to terminate it."
      re1   = ("What is the power of the Loop?" , Choice "Russell: for you, if you put it on, you become referentially transparent. For the Imperator, there is no end to its power. If he gets it in his possession, he will vanquish us all." [re2])
      re2   = ("Let's go!" , Action "Let's put our team together and head for Error." (updateMap (connect 1 0) . add ["Bertrand Russell"] . removeHere ["Bertrand Russell"]) )
      in Choice intro [re1,re2]
      ) ( Branch ( (==7).here) (end "Russell: Let me speak to him and Brouwer."
      ) (end "Russell: We should put our team together and head for Error." ) )
    )
  , ( ["Arend Heyting"] , Choice "Heyting: What can I get you?"
      [ ( "A pint of Ex Falso Quodbibet, please." , end "There you go." )
      , ( "The Hop Erat Demonstrandum, please."   , end "Excellent choice." )
      , ( "Could I get a Maltus Ponens?"          , end "Mind, that's a strong one." )
      ]
    )
  , ( ["Luitzen Brouwer"] , Branch (isAt 1 "Luitzen Brouwer")
      ( Choice "Brouwer: Haircut?"
        [ ( "Please." , let
          intro = "Brouwer is done and holds up the mirror. You notice that one hair is standing up straight."
          r1 i  = ( "There's just this one hair sticking up. Could you comb it flat, please?" , d i)
          r2    = ( "Thanks, it looks great." , end "Brouwer: You're welcome.")
          d  i  | i == 0    = Choice intro [r2]
                | otherwise = Choice intro [r1 (i-1),r2]
        in d 100)
        , ( "Actually, could you do a close shave?" , end "Of course. I shave everyone who doesn't shave themselves." )
        , ( "I'm really looking for help." , Choice "Brouwer: Hmmm. What with? Is it mysterious?"
          [ ( "Ooh yes, very. And dangerous." , Action "Brouwer: I'm in!" (add ["Luitzen Brouwer"] . removeHere ["Luitzen Brouwer"]) )
          ] )
        ]
      )
      ( end "Nothing" )
    )
  , ( ["David Hilbert"] , Branch (not . isconn 2 3) (let
        intro = "You wait your turn in the queue. The host, David Hilbert, puts up the first guest in Room 1, and points the way to the stairs.\n\nYou seem to hear that the next couple are also put up in Room 1. You decide you must have misheard. It is your turn next.\n\nHilbert: Lodging and breakfast? Room 1 is free."
        re1   = ("Didn't you put up the previous guests in Room 1, too?" , Choice "Hilbert: I did. But everyone will move up one room to make room for you if necessary. There is always room at the Hilbert Hotel & Resort." [("But what about the last room? Where do the guests in the last room go?" , Choice "Hilbert: There is no last room. There are always more rooms." [("How can there be infinite rooms? Is the hotel infinitely long?" , Choice "Hilbert: No, of course not! It was designed by the famous architect Zeno Hadid. Every next room is half the size of the previous." [re2])])])
        re2   =  ("Actually, I am looking for someone." , Action "Hilbert: Yes, someone is staying here. You'll find them in Room n+1. Through the doors over there, up the stairs, then left." (updateMap (connect 2 3)))
      in Choice intro [re1,re2]
      ) (end "Hilbert seems busy. You hear him muttering to himself: Problems, problems, nothing but problems. You decide he has enough on his plate and leave." )
    )
  , ( ["William Howard"] ,  Branch (isAt 3 "William Howard")
      (Choice "Howard: Yes? Are we moving up again?" [("Quick, we need your help. We need to travel to Error." , Action "Howard: Fine. My bags are packed anyway, and this room is tiny. Let's go!" (add ["William Howard"] . removeAt 3 ["William Howard"]))]
      ) (Branch (isAt 6 "William Howard") (Choice "Howard: What can I get you?"
        [ ("The Lambda Rogan Josh with the Raita Monad for starter, please." , end "Coming right up.")
        , ("The Vindaloop with NaN bread on the side." , Choice "Howard: It's quite spicy." [("I can handle it." , end "Excellent." ) ] )
        , ("The Chicken Booleani with a stack of poppadums, please.", end "Good choice." )
        ]
      ) (end "Howard: We need to find Curry. He'll know the way.")
    ) )
  , ( ["Jean-Yves Girard"] , Branch (isconn 4 5)  (end "You have seen enough here.") (Action "Raised on a large platform in the centre of the temple, Girard is preaching the Linearity Gospel. He seems in some sort of trance, so it is hard to make sense of, but you do pick up some interesting snippets. `Never Throw Anything Away' - you gather they must be environmentalists - `We Will Solve Church's Problems', `Only This Place Matters'... Perhaps, while he is speaking, now is a good time to take a peek behind the temple..." (updateMap (connect 4 5) ))
    )
  , ( ["Vending machine"] , Choice "The walls of the Temple of Linearity are lined with vending machines. Your curiosity gets the better of you, and you inspect one up close. It sells the following items:"
      [ ( "Broccoli"  , end "You don't like broccoli." )
      , ( "Mustard"   , end "It might go with the broccoli." )
      , ( "Watches"   , end "They seem to have a waterproof storage compartment. Strange." )
      , ( "Camels"    , end "You don't smoke, but if you did..." )
      , ( "Gauloises" , end "You don't smoke, but if you did..." )
      ]
    )
  , ( ["Jean-Louis Krivine"] , end "Looking through the open kitchen door, you see the chef doing the dishes. He is rinsing and stacking plates, but it's not a very quick job because he only has one stack. You also notice he never passes any plates to the front. On second thought, that makes sense - it's a takeaway, after all, and everything is packed in cardboard boxes. He seems very busy, so you decide to leave him alone."
    )
  , ( ["Haskell Curry"] , Branch (isAt 6 "Haskell Curry")
      (Choice "Curry: What can I get you?"
        [ ("The Lambda Rogan Josh with the Raita Monad for starter, please." , end "Coming right up.")
        , ("The Vindaloop with NaN bread on the side." , Choice "Curry: It's quite spicy." [("I can handle it." , end "Excellent." ) ] )
        , ("The Chicken Booleani with a stack of poppadums, please.", end "Good choice." )
        , ("Actually, I am looking for help getting to Error." , end "Curry: Hmm. I may be able to help, but I'll need to speak to William Howard.")
        ]
      ) (end "Nothing")
    )
  , ( ["Haskell Curry","William Howard"] , Branch (not . isconn 6 7) (Action "Curry:  You know the way to Error, right?\nHoward: I thought you did?\nCurry:  Not really. Do we go via Computerborough?\nHoward: Yes, I think so. Is that along the I-50?\nCurry:  Yes, third exit. Shall I go with them?\nHoward: sure. I can watch the shop while you're away." (add ["Haskell Curry"] . removeAt 6 ["Haskell Curry"] . addAt 6 ["William Howard"] . remove ["William Howard"] . updateMap (connect 6 7) )) (end "It's easy, just take the third exit on I-50.")
    )
  , ( ["Gottlob Frege"] , end "A person who appears to be the leader of the mob approaches your vehicle. When he gets closer, you recognise him as Gottlob Frege. You start backing away, and he starts yelling at you.\n\nFrege: Give us the Loop! We can control it! We can wield its power!\n\nYou don't see a way forward. Perhaps Russell has a plan." )
  , ( ["Bertrand Russell","Gottlob Frege","Luitzen Brouwer"] , let
        intro = "Frege is getting closer, yelling at you to hand over the Loop, with the mob on his heels, slowly surrounding you. The tension in the car is mounting. But Russell calmly steps out to confront Frege.\n\nRussell:"
        re1   = ( "You cannot control its power! Even the very wise cannot see all ends!" , Choice "Frege: I can and I will! The power is mine!\n\nRussell:" [re2,re3] )
        re2   = ( "Brouwer, whom do you shave?" , Choice "Brouwer: Those who do not shave themselves. Obviously. Why?\n\nRussell:" [re3] )
        re3   = ( "Frege, answer me this: DOES BROUWER SHAVE HIMSELF?" , Action
                  "Frege opens his mouth to shout a reply. But no sound passes his lips. His eyes open wide in a look of bewilderment. Then he looks at the ground, and starts walking in circles, muttering to himself and looking anxiously at Russell. The mob is temporarily distracted by the display, uncertain what is happening to their leader, but slowly enclosing both Frege and Russell. Out of the chaos, Russell shouts:\n\nDRIVE, YOU FOOLS!\n\nYou floor it, and with screeching tires you manage to circle around the mob. You have made it across.\n\nEND OF ACT 1. To be continued..."
                  (const Over)
                )
      in Choice intro [re1,re2,re3]
    )
  , ( ["Bertrand Russell","Haskell Curry","Luitzen Brouwer"] , Branch ((==7).here) (end "Road trip! Road trip! Road trip!") (end "Let's head for Error!")
    )
  ]

