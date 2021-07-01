def pytest_addoption(parser):
    parser.addoption("--valgrind", action="store_const", const=True, default=False)

def pytest_generate_tests(metafunc):
    option_value = metafunc.config.option.valgrind
    if 'valgrind' in metafunc.fixturenames and option_value is not None:
        metafunc.parametrize("valgrind", [option_value])
