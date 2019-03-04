struct CalculationResult {
    hyper  result;
    unsigned hyper startMillis;
    unsigned hyper finishMillis;
};

program CALCULATOR {
    version CALCULATORVERS {
        CalculationResult add(hyper, hyper) = 1;
        hyper addSimple(hyper, hyper) = 2;
    } = 1;
} = 117;