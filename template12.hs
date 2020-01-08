import Data.List
import Data.Maybe

type Id = String

type State = Int

type Transition = ((State, State), Id)

type LTS = [Transition]

type Alphabet = [Id]

data Process = STOP | Ref Id | Prefix Id Process | Choice [Process] 
             deriving (Eq, Show)

type ProcessDef = (Id, Process)

type StateMap = [((State, State), State)]

------------------------------------------------------
-- PART I

lookUp :: Eq a => a -> [(a, b)] -> b
--Pre: The item is in the table
lookUp
  = ((.).(.)) fromJust lookup

states :: LTS -> [State]
states lts 
  = nub (concat (map states' lts))
  where
    states' ((s1, s2), id)
      = [s1, s2]

transitions :: State -> LTS -> [Transition]
transitions st [] 
  = []
transitions st (((s1, s2), id) : lts) 
  | s1 == st  = (((s1, s2), id) : (transitions st lts))
  | otherwise = transitions st lts

alphabet :: LTS -> Alphabet
alphabet 
  = (.) nub (map snd)

------------------------------------------------------
-- PART II

actions :: Process -> [Id]
actions STOP
  = []
actions (Ref _)
  = []
actions (Prefix id pr)
  = id : (actions pr)
actions (Choice prs)
  = concat (map actions prs)

accepts :: [Id] -> [ProcessDef] -> Bool
--Pre: The first item in the list of process definitions is
--     that of the start process.
accepts [] _
  = True
accepts _ ((_, STOP) : defs)
  = False
accepts ids ds@((_, Ref ref) : defs)
  = accepts' ids ds (lookUp ref defs) 
accepts (id : ids) ds@(((_, Prefix nm pr)) : defs)
  | id ==  nm = accepts' ids ds pr
  | otherwise = False 
accepts ids ds@(((_, Choice prs)) : def)
  = or (map (accepts' ids ds) prs)

accepts' :: [Id] -> [ProcessDef] -> Process -> Bool
accepts' [] _ _
  = True
accepts' _ _ STOP
  = False
accepts' ids defs (Ref ref)
  = accepts' ids defs (lookUp ref defs)
accepts' (id : ids) defs (Prefix name pr)
  | id == name = accepts' ids defs pr
  | otherwise  = False
accepts' ids defs (Choice prs)
  = or (map (accepts' ids defs) prs)
  

------------------------------------------------------
-- PART III

--composeTransitions :: Transition -> Transition 
--                   -> Alphabet -> Alphabet 
--                   -> StateMap 
--                   -> [Transition]
--Pre: The first alphabet is that of the LTS from which the first transition is
--     drawn; likewise the second.
--Pre: All (four) pairs of source and target states drawn from the two transitions
--     are contained in the given StateMap.
composeTransitions ((s, t) , a) ((s', t'), a') alpha1 alpha2 stmap 
  | a == a'                         = [((start, lookUp (t, t') stmap), a)]
  | elem a alpha2 && elem a' alpha1 = []
  | elem a' alpha1                  = [((start, lookUp (t, s') stmap), a)]
  | elem a alpha2                   = [((start, lookUp (s, t') stmap), a')]
  | otherwise                       = [((start, lookUp (t, s') stmap), a), ((start, lookUp (s, t') stmap), a')]
    where
        start = lookUp (s, s') stmap

pruneTransitions :: [Transition] -> LTS
pruneTransitions trs
  = nub (visit 0 [])   
  where
    visit :: State -> [State] -> [Transition]
    visit st visited 
      | not (elem st visited) = (transitions st trs) 
                                ++ (concat (map helper (transitions st trs)))
      | otherwise             = []
      where
        helper ((s, t), id)
          = visit t (st : visited)

------------------------------------------------------
-- PART IV

compose :: LTS -> LTS -> LTS
compose lts1 lts2
  = pruneTransitions (concat (map helper (cartPro st1 st2)))
  where
    st1 = states lts1
    st2 = states lts2
    al1 = alphabet lts1
    al2 = alphabet lts2
    i = zip (cartPro st1 st2) [0..]
    helper (s, s')
      = concat (map helper' (cartPro (transitions s lts1) (transitions s lts2)))
    helper' (tr1, tr2)
      = composeTransitions tr1 tr2 al1 al2 i

cartPro alpha1 alpha2
  = [(s, s') | s <- alpha1, s' <- alpha2]

------------------------------------------------------
-- PART V

buildLTS :: [ProcessDef] -> LTS
-- Pre: All process references (Ref constructor) have a corresponding
--      definition in the list of ProcessDefs.
buildLTS 
  = undefined

------------------------------------------------------
-- Sample process definitions...

vendor, clock, play, maker, user, p, q, switch, off, on :: ProcessDef

vendor 
  = ("VENDOR", Choice [Prefix "red"  (Prefix "coffee" (Ref "VENDOR")),
                       Prefix "blue" (Prefix "tea" (Ref "VENDOR")),
                       Prefix "off" STOP])

clock 
  = ("CLOCK", Prefix "tick" (Prefix "tock" (Ref "CLOCK")))

play 
  = ("PLAY", Choice [Prefix "think" (Prefix "move" (Ref "PLAY")), 
                     Prefix "end" STOP])

maker 
  = ("MAKER", Prefix "make" (Prefix "ready" (Ref "MAKER")))

user  
  = ("USER",  Prefix "ready" (Prefix "use" (Ref "USER")))

p = ("P", Prefix "a" (Prefix "b" (Prefix "c" STOP)))

q = ("Q",  Prefix "d" (Prefix "c" (Prefix "b" (Ref "Q"))))

switch 
  = ("SWITCH", Ref "OFF")

off 
  = ("OFF", Choice [Prefix "on" (Ref "ON")])

on  
  = ("ON",  Choice [Prefix "off" (Ref "OFF")])

------------------------------------------------------
-- Sample LTSs...

vendorLTS, clockLTS, playLTS, clockPlayLTS, makerLTS, userLTS, makerUserLTS, 
  pLTS, qLTS, pqLTS, switchLTS :: LTS

vendorLTS 
  = [((0,1),"off"),((0,2),"blue"),((0,3),"red"),((2,0),"tea"),((3,0),"coffee")]

clockLTS 
  = [((0,1),"tick"),((1,0),"tock")]

playLTS 
  = [((0,1),"end"),((0,2),"think"),((2,0),"move")]

clockPlayLTS 
  = [((0,1),"end"),((1,4),"tick"),((4,1),"tock"),((0,3),"tick"),
     ((3,4),"end"),((3,0),"tock"),((3,5),"think"),((5,3),"move"),
     ((5,2),"tock"),((2,0),"move"),((2,5),"tick"),((0,2),"think")]

makerLTS 
  = [((0,1),"make"),((1,0),"ready")]

userLTS 
  = [((0,1),"ready"),((1,0),"use")]

makerUserLTS 
  = [((0,2),"make"),((2,1),"ready"),((1,0),"use"),((1,3),"make"),((3,2),"use")]

pLTS 
  = [((0,1),"a"),((1,2),"b"),((2,3),"c")]

qLTS 
  = [((0,1),"d"),((1,2),"c"),((2,0),"b")]

pqLTS 
  = [((0,1),"d"),((1,4),"a"),((0,3),"a"),((3,4),"d")]

switchLTS 
  = [((0,1),"on"),((1,0),"off")]

