for i in range(1, 8):
    name = "T15732_%02d" % i
    test(name, normal, run_command, ['$MAKE -s --no-print-directory {}'.format(name)])
