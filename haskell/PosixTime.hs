import System.Directory
import System.Time
import System.Posix.Types

main = do
	now <- getClockTime
	toCalendarTime now >>= print
	getModificationTime "D:/hello.py"