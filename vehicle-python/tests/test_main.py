def base_version(version: str) -> str:
    from packaging.version import Version

    return Version(version).base_version


def test_main() -> None:
    from vehicle_lang import VERSION, session

    exc, out, err, log = session.check_output(["--version"])
    assert isinstance(exc, int) and exc == 0
    assert isinstance(out, str) and base_version(out) == base_version(VERSION)
    assert err is None
    assert log is None
