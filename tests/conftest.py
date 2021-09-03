def pytest_addoption(parser):
    parser.addoption("--valgrind", action="store_const", const=True, default=False)
    parser.addoption("--manager", action="store", default="None")

def pytest_generate_tests(metafunc):
    option_value = metafunc.config.option.valgrind
    if 'valgrind' in metafunc.fixturenames and option_value is not None:
        metafunc.parametrize("valgrind", [option_value])

    option_value = metafunc.config.option.manager
    if 'manager' in metafunc.fixturenames and option_value is not None:
        metafunc.parametrize("manager", [option_value])
