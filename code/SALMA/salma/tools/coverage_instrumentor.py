from pathlib import Path
import re

PATTERN_D = re.compile(r"/\*\s*\$D\$\s*\*/")
PATTERN_DS = re.compile(r"/\*\s*\$DS\$\s*\*/")
POSITIONS = dict()

ID_COUNTER = 0


def instrumentFile(src: Path, dest: Path):
    destFile = dest / src.name
    print("instrumenting {} -> {}".format(src, destFile))

    with src.open() as fsrc:
        with destFile.open("w") as fdest:
            lnum = 1

            def makerepl_D(m):
                global ID_COUNTER, POSITIONS
                ID_COUNTER += 1
                POSITIONS[ID_COUNTER] = (fsrc.name, lnum)
                return "logdec({}), ".format(ID_COUNTER)

            def makerepl_DS(m):
                global ID_COUNTER, POSITIONS
                ID_COUNTER += 1
                POSITIONS[ID_COUNTER] = (fsrc.name, lnum)
                return ", logdec({}) ".format(ID_COUNTER)

            for line in fsrc:
                l2 = PATTERN_D.sub(makerepl_D, line)
                l2 = PATTERN_DS.sub(makerepl_DS, l2)
                fdest.write(l2)
                lnum += 1


if __name__ == '__main__':
    srcPath = Path("ecl-src")
    destPath = Path("ecl-src-instrumented")
    if not destPath.exists():
        destPath.mkdir()
    for f in srcPath.glob("*.ecl"):
        instrumentFile(f, destPath)

    posPath = destPath / "positions.csv"
    with posPath.open("w") as fpos:
        for (pid, (fname, pos)) in POSITIONS.items():
            fpos.write("{};{};{}\n".format(pid, fname, pos))


