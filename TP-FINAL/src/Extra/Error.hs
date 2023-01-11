module Extra.Error 
    (Error (..))
    where

-- Estructura para errores
data Error = WrongDateFormat 
           | BadPriority
           | FilterError String
           | DirNotFound String 
           | TaskNotFound String 
           | TaskAlreadyExists String 
           | DirAlreadyExists String
           | CannotEditRootDir
           | CannotCreateTaskInRootDir
           | HappyError String
           | ProfileDoesNotExists String
           | ProfileAlreadyExists String
           | ErrorLoadingProfile String
           | CannotDeleteDefaultProfile
    deriving (Eq)

instance Show Error where
    show WrongDateFormat = "Wrong date format: YYYY-MM-DD [HH:MM]"
    show BadPriority = "Bad priority: must be >= -1"
    show (FilterError s) = "Filter error: " ++ s
    show (DirNotFound s) = "Directory not found: " ++ s
    show (TaskNotFound s) = "Task not found: " ++ s
    show (TaskAlreadyExists s) = "Task already exists: " ++ s
    show (DirAlreadyExists s) = "Directory already exists: " ++ s
    show CannotEditRootDir = "Cannot edit root directory"
    show CannotCreateTaskInRootDir = "Cannot create task in root directory"
    show (ProfileDoesNotExists s) = "Profile does not exists: " ++ s
    show (ProfileAlreadyExists s) = "Profile already exists: " ++ s
    show (ErrorLoadingProfile s) = "Error loading profile: " ++ s
    show CannotDeleteDefaultProfile = "Cannot delete default profile"