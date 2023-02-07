import subprocess

EXPECTED_OUTPUT_INDEX = 1
TEST_NAME_INDEX = 0


test_identifiers_expected_output = [
    "TOKEN IDENTIFIER \"andy\" (ID \"andy\") 1",
    "TOKEN IDENTIFIER \"formless\" (ID \"formless\") 1",
    "TOKEN IDENTIFIER \"fo\" (ID \"fo\") 1",
    "TOKEN IDENTIFIER \"_\" (ID \"_\") 1",
    "TOKEN IDENTIFIER \"_123\" (ID \"_123\") 1",
    "TOKEN IDENTIFIER \"_abc\" (ID \"_abc\") 1",
    "TOKEN IDENTIFIER \"ab123\" (ID \"ab123\") 1",
    "TOKEN IDENTIFIER \"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_\" (ID \"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890_\") 1",
    "TOKEN EOF \"\" NONE 1"
]

test_keywords_expected_output = [
    "TOKEN AND \"and\" NONE 1",
    "TOKEN CLASS \"class\" NONE 1",
    "TOKEN ELSE \"else\" NONE 1",
    "TOKEN FALSE \"false\" FALSE_LIT 1",
    "TOKEN FOR \"for\" NONE 1",
    "TOKEN FUN \"fun\" NONE 1",
    "TOKEN IF \"if\" NONE 1",
    "TOKEN NIL \"nil\" NIL_LIT 1",
    "TOKEN OR \"or\" NONE 1",
    "TOKEN RETURN \"return\" NONE 1",
    "TOKEN SUPER \"super\" NONE 1",
    "TOKEN THIS \"this\" NONE 1",
    "TOKEN TRUE \"true\" TRUE_LIT 1",
    "TOKEN VAR \"var\" NONE 1",
    "TOKEN WHILE \"while\" NONE 1",
    "TOKEN EOF \"\" NONE 1"
]

test_numbers_expected_output = [
    "TOKEN NUMBER \"123\" (NUM 123.0) 1",
    "TOKEN NUMBER \"123.456\" (NUM 123.456) 1",
    "TOKEN DOT \".\" NONE 1",
    "TOKEN NUMBER \"456\" (NUM 456.0) 1",
    "TOKEN NUMBER \"123\" (NUM 123.0) 1",
    "TOKEN DOT \".\" NONE 1",
    "TOKEN EOF \"\" NONE 1"
]

test_punctuators_expected_output = [
    "TOKEN LEFT_PAREN \"(\" NONE 1",
    "TOKEN RIGHT_PAREN \")\" NONE 1",
    "TOKEN LEFT_BRACE \"{\" NONE 1",
    "TOKEN RIGHT_BRACE \"}\" NONE 1",
    "TOKEN SEMICOLON \";\" NONE 1",
    "TOKEN COMMA \",\" NONE 1",
    "TOKEN PLUS \"+\" NONE 1",
    "TOKEN MINUS \"-\" NONE 1",
    "TOKEN STAR \"*\" NONE 1",
    "TOKEN BANG_EQUAL \"!=\" NONE 1",
    "TOKEN EQUAL_EQUAL \"==\" NONE 1",
    "TOKEN LESS_EQUAL \"<=\" NONE 1",
    "TOKEN GREATER_EQUAL \">=\" NONE 1",
    "TOKEN BANG_EQUAL \"!=\" NONE 1",
    "TOKEN LESS \"<\" NONE 1",
    "TOKEN GREATER \">\" NONE 1",
    "TOKEN SLASH \"/\" NONE 1",
    "TOKEN DOT \".\" NONE 1",
    "TOKEN EOF \"\" NONE 1"
]

test_strings_expected_output = [
    "TOKEN STRING \"string\" (STR \"string\") 1",
    "TOKEN STRING \"second row \\n third row\" (STR \"second row \\n third row\") 2",
    "TOKEN EOF \"\" NONE 3"
]


tests = [("TestIdentifiers.hs", test_identifiers_expected_output),
         ("TestKeywords.hs" , test_keywords_expected_output),
         ("TestNumbers.hs" , test_numbers_expected_output),
         ("TestPunctuators.hs" , test_punctuators_expected_output),
         ("TestStrings.hs" , test_strings_expected_output)]



def run_test(test_name):
    # Run Tester and capture output
    result = subprocess.run(["runhaskell", "./tests/" + str(test_name)], capture_output=True, text=True)
    output = result.stdout
    error = result.stderr
    
    # Print the output and error
    #print(output)
    print(error, end='')

    return output


def print_red(input_string):
    """ print_red - print a given string to the
        in red without a new line.
        
        @param input_string The string to print in red
    """
    print("\033[91m {}\033[00m".format(input_string), end = '')
 
 
def print_green(input_string):
    """ print_green - print a given string to the
        in green without a new line.

        @param input_string The string to print in green
    """
    print("\033[92m {}\033[00m" .format(input_string), end = '')


if __name__ == "__main__":
    # Get output lines from test
    #output_lines = run_tests().splitlines()

    # Validate output
    num_errors = 0
    num_passing = 0
    for test in tests:
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
        
        print_green("PASSED ")
        print(test[TEST_NAME_INDEX])




















    """
    for expected_line in expected_output:
        if output_lines[i] != expected_line:
            print_red("ERROR")
            print("  Got: " + str(output_lines[i]) + "\nExpected: " + str(expected_line))
            num_errors = num_errors + 1
        else:
            print_green("PASSED")
            print(" Output: " + str(output_lines[i]))
            num_passing = num_passing + 1
        i = i + 1

    print("\n\n" + str(num_passing) + " tests passed.\n" + str(num_errors) + " tests failed.\n")
    """