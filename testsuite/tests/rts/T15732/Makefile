TOP=../../..
include $(TOP)/mk/boilerplate.mk
include $(TOP)/mk/test.mk

# We can set RTS options using response files. This test sets a GC flag.
T15732_01:
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make -rtsopts T15732_01.hs
	./T15732_01 @T15732_01.response

# If the program was not compiled with -rtsopts and we pass RTS options
# at runtime, an error should be reported.
T15732_02:
	'$(TEST_HC)' $(filter-out -rtsopts, $(TEST_HC_OPTS)) -v0 --make T15732_02.hs
	./T15732_02 @T15732_02.response &> /dev/null

# Recursive response files are OK. Also, @file args can appear in between
# +RTS...-RTS blocks.
T15732_03:
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make T15732_03.hs
	./T15732_03 @T15732_03.response

# RTS options occuring after --RTS are ignored, but response file arguments are
# still expanded.
T15732_04:
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make T15732_04.hs
	./T15732_04 @T15732_04.response

# getArgs and getArgsWithResponseFiles are distinct.
T15732_05:
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make T15732_05.hs
	./T15732_05 a b c @T15732_05.response

# -no-expand-response-files disables response file processing, but RTS options are still
# handled correctly.
T15732_06:
	'$(TEST_HC)' $(TEST_HC_OPTS) -no-expand-response-files -v0 --make T15732_06.hs
	./T15732_06 a b c @T15732_06.response +RTS -F3 -RTS

# Tests from the GCC source-code.
T15732_07:
	'$(TEST_HC)' $(TEST_HC_OPTS) -v0 --make T15732_07.hs
	./T15732_07 @T15732_07.response

.PHONY: T15732_01 T15732_02 T15732_03 T15732_04 T15732_05 T15732_06 T15732_07
