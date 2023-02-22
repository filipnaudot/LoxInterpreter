import subprocess

EXPECTED_OUTPUT_INDEX = 1
TEST_NAME_INDEX = 0



##########################################################################
######################### Define expected output #########################
##########################################################################
test_leteral_expected_output = [
    "8",
    "TRUE_LIT;",
    "FALSE_LIT;",
    "5.0;",
    "5.111;",
    "a;",
    "_ab;",
    "NIL_LIT;",
    "\"string\";",
]

test_expr_expected_output = [
    "8",
    "(5.0*2.0);",
    "(TRUE_LIT&&FALSE_LIT);",
    "(-2.0);",
    "(!TRUE_LIT);",
    "((2.0==3.0));",
    "(2.0+(3.0*5.0));",
    "d=(((2.0+2.0))+(2.0*2.0));",
    "_e=(2.0+(2.0+(2.0*2.0)));",
]

test_assignment_expected_output = [
    "6",
    "variable22=\"hello\";",
    "a=(5.0*2.0);",
    "b=TRUE_LIT;",
    "c=FALSE_LIT;",
    "d=NIL_LIT;",
    "(a==b);",
]

test_stmt_expected_output = [
    "5",
    "if(a)(5.0*2.0);",
    "if(a){(5.0*2.0);}",
    "if(a)(5.0*2.0);else99.0;",
    "if(a){(5.0*2.0);}else{99.0;}",
    "{5.0; 7.0; 9.0;}",
]

test_var_declaration_expected_output = [
    "6",
    "V DEC -> a=1.0;",
    "V DEC -> b=TRUE_LIT;",
    "V DEC -> c=FALSE_LIT;",
    "V DEC -> str=\"hello\";",
    "V DEC -> d=((2.0/7.0));",
    "V DEC -> empty;",
]

test_program_expected_output = [
    "1",
    "if((a<5.0)){printg; 88.0;}else{if(FALSE_LIT){while(a=5.0)return;}}",
]

test_fibonacci_program_expected_output = [
    "4",
    "V DEC -> a=0.0;",
    "V DEC -> temp;",
    "V DEC -> b=1.0;",
    "while((a<10000.0)){printa; temp=a; a=b; b=(temp+b);}",
]


tests = [
    ("TestLiteral.hs", test_leteral_expected_output),
    ("TestExpr.hs", test_expr_expected_output),
    ("TestAssignment.hs", test_assignment_expected_output),
    ("TestStmt.hs", test_stmt_expected_output),
    ("TestVarDecl.hs", test_var_declaration_expected_output),
    ("TestProg.hs", test_program_expected_output),
    ("TestFibProg.hs", test_fibonacci_program_expected_output),
    ]






##########################################################################
########################### Test functions ###############################
##########################################################################
def run_test(test_name):
    """ run_test - Runs a given test in a subprocess and
                   captures the output.
        
        @param test_name The name of test test file.
    """
    # Run test and capture output
    result = subprocess.run(["runhaskell", "./tests/parser/" + str(test_name)], capture_output=True, text=True)
    output = result.stdout
    error = result.stderr
    
    # Print the output and error
    #print(output)
    print(error, end='')

    return output


def print_red(input_string):
    """ print_red - Print a given string to the terminal
        in red without a new line.
        
        @param input_string The string to print in red.
    """
    print("\033[91m {}\033[00m".format(input_string), end = '')
 
 
def print_green(input_string):
    """ print_green - Print a given string to the terminal
        in green without a new line.

        @param input_string The string to print in green.
    """
    print("\033[92m {}\033[00m" .format(input_string), end = '')


if __name__ == "__main__":
    for test in tests:
        num_errors = 0
        num_passing = 0
        i = 0

        output_lines = run_test(test[TEST_NAME_INDEX]).splitlines()

        for expected_line in test[EXPECTED_OUTPUT_INDEX]:
            if output_lines[i] != expected_line:
                print_red("ERROR")
                print("  Got: " + str(output_lines[i]) + "\n\tExpected: " + str(expected_line))
                num_errors = num_errors + 1
            else:
                num_passing = num_passing + 1
            i = i + 1
        
        if num_errors == 0:
            print_green("PASSED ")
            print(test[TEST_NAME_INDEX])
        else:
            print_red("FAILD ")
            print(test[TEST_NAME_INDEX])