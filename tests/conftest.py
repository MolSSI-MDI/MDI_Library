def pytest_addoption(parser):
    parser.addoption("--valgrind", action="store_const", const=True, default=False)
    parser.addoption("--manager", action="store", default="None")
    parser.addoption("--driver_dir", action="store", default="../build")
    parser.addoption("--engine_dir", action="store", default="../build")

def pytest_generate_tests(metafunc):
    option_value = metafunc.config.option.valgrind
    if 'valgrind' in metafunc.fixturenames and option_value is not None:
        metafunc.parametrize("valgrind", [option_value])

    option_value = metafunc.config.option.manager
    if 'manager' in metafunc.fixturenames and option_value is not None:
        metafunc.parametrize("manager", [option_value])

    option_value = metafunc.config.option.driver_dir
    if 'driver_dir' in metafunc.fixturenames and option_value is not None:
        metafunc.parametrize("driver_dir", [option_value])

    option_value = metafunc.config.option.engine_dir
    if 'engine_dir' in metafunc.fixturenames and option_value is not None:
        metafunc.parametrize("engine_dir", [option_value])
