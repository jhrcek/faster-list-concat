module ListConcat exposing (main)

import Benchmark exposing (Benchmark, benchmark, describe)
import Benchmark.Runner exposing (BenchmarkProgram)


main : BenchmarkProgram
main =
    Benchmark.Runner.program suite


suite : Benchmark
suite =
    describe "List.concat vs fastConcat" <|
        List.concatMap
            (\( listCount, listLength ) ->
                [ makeBenchmark
                    { benchmarkName = "List.concat (List (List Int))"
                    , testedFunction = List.concat
                    , listCount = listCount
                    , listLength = listLength
                    , listToConcatenate = mkIntList listCount listLength
                    }
                , makeBenchmark
                    { benchmarkName = "fastConcat (List (List Int))"
                    , testedFunction = fastConcat
                    , listCount = listCount
                    , listLength = listLength
                    , listToConcatenate = mkIntList listCount listLength
                    }
                , makeBenchmark
                    { benchmarkName = "List.concat (List (List Person))"
                    , testedFunction = List.concat
                    , listCount = listCount
                    , listLength = listLength
                    , listToConcatenate = mkPersonList listCount listLength
                    }
                , makeBenchmark
                    { benchmarkName = "fastConcat (List (List Person))"
                    , testedFunction = fastConcat
                    , listCount = listCount
                    , listLength = listLength
                    , listToConcatenate = mkPersonList listCount listLength
                    }
                ]
            )
            [ ( 10, 1000 ), ( 100, 100 ), ( 1000, 10 ) ]


type alias Config a =
    { benchmarkName : String
    , testedFunction : List (List a) -> List a
    , listCount : Int
    , listLength : Int
    , listToConcatenate : List (List a)
    }


makeBenchmark : Config a -> Benchmark
makeBenchmark { benchmarkName, testedFunction, listCount, listLength, listToConcatenate } =
    benchmark (benchmarkName ++ " - " ++ String.fromInt listCount ++ " lists of length " ++ String.fromInt listLength) <|
        \() -> testedFunction listToConcatenate


fastConcat : List (List a) -> List a
fastConcat =
    List.foldr (++) []


mkIntList : Int -> Int -> List (List Int)
mkIntList m n =
    List.repeat m (List.range 1 n)


type alias Person =
    { name : String, age : Int }


joe : Person
joe =
    { name = "Joe", age = 50 }


mkPersonList : Int -> Int -> List (List Person)
mkPersonList m n =
    List.repeat m (List.repeat n joe)
