import Test.Examples as Ex
import Test.Dag as Dag
import Test.Framework

main = defaultMain allTests

allTests =    
    [ testGroup "Examples" Ex.tests
    , testGroup "Dag" Dag.tests
    ]
