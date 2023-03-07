import Test.Examples as Ex
import Test.Dag as Dag
import Test.Multi.Examples as MEx
import Test.Multi.Dag as MDag
import Test.Framework

main = defaultMain allTests

allTests =    
    [ testGroup "Examples" Ex.tests
    , testGroup "Dag" Dag.tests
    , testGroup "MultiExamples" MDag.tests
    , testGroup "MultiDag" MEx.tests
    ]
