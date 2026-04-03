$NoPrefix
Option Explicit
Randomize Timer
Dim Shared HostOS As String
OSPROBE
On Error GoTo ErrorHandler
Screen NewImage(640, 480, 32)
$Console
Console Off

'Directory Path Constants
$If WIN Then
    Const LibraryDirectory = "Library\"
    Const CommonDirectory = "Common\"
    Const UncommonDirectory = "Uncommon\"
    Const RareDirectory = "Rare\"
    Const MythicDirectory = "Mythic\"
    Const BonusDirectory = "Bonus\"
    Const LandDirectory = "Land\"
    Const WeightsFile = "Weights"

    Const PackDirectory = "Pack\"

    Const SettingsDirectory = "Settings\"
    Const SettingsFile = "Settings"

    const TemplateLibrary = "Template\"

    const DirSlash = "\"
$End If
$If LINUX Then
    Const LibraryDirectory = "Library/"
    Const CommonDirectory = "Common/"
    Const UncommonDirectory = "Uncommon/"
    Const RareDirectory = "Rare/"
    Const MythicDirectory = "Mythic/"
    Const BonusDirectory = "Bonus/"
    Const LandDirectory = "Land/"
    Const WeightsFile = "Weights"

    Const PackDirectory = "Pack/"

    Const SettingsDirectory = "Settings/"
    Const SettingsFile = "Settings"

    Const TemplateLibrary = "Template/"

    Const DirSlash = "/"
$End If
$If MAC Then
    Const LibraryDirectory = "Library/"
    Const CommonDirectory = "Common/"
    Const UncommonDirectory = "Uncommon/"
    Const RareDirectory = "Rare/"
    Const MythicDirectory = "Mythic/"
    Const BonusDirectory = "Bonus/"
    Const LandDirectory = "Land/"
    Const WeightsFile = "Weights"

    Const PackDirectory = "Pack/"

    Const SettingsDirectory = "Settings/"
    Const SettingsFile = "Settings"

    const TemplateLibrary = "Template/"

    const DirSlash = "/"
$End If

Dim CLA
Dim Shared DebugMode
For CLA = 0 To CommandCount
    Select Case LCase$(Command$)
        Case "debug"
            DebugMode = 1
            Console On
    End Select
Next

'Global Variables
Dim SessionString As String
Select Case HostOS
    Case "Mac OS", "Linux"
        SessionString = Date$ + "_" + Time$
    Case "Windows"
        SessionString = Date$ + "_" + LTrim$(RTrim$(Str$(Int(Timer))))
End Select

Const VersionString = "V1.6b"

Const Wsize = 5
Const R_Land = 0
Const R_common = 1
Const R_uncommon = 2
Const R_rare = 3
Const R_mythic = 4
Const R_bonus = 5

Dim Weights(Wsize) As Single

Const Ssize = 2
Const S_PackSize = 0
Const S_PackCount = 1
Const S_LandCount = 2


Dim Shared Settings(Ssize) As Single
Dim Shared Settings_LibraryTarget As String

Dim Shared SelectionMode

Dim Shared ErrorReturn As String

'MOVED FROM build library array
Dim Shared DummyString As String
Dim RarityCount As Single
Dim LibraryMaxSize(Wsize) As Single
Dim LibArrayMaxSize As Single
Dim Shared LibraryTotalSize
Dim i, ii



'default weights
Weights(R_Land) = 0
Weights(R_common) = 100
Weights(R_uncommon) = 50
Weights(R_rare) = 25
Weights(R_mythic) = 10
Weights(R_bonus) = 0

Settings(S_PackSize) = 10
Settings(S_PackCount) = 1
Settings(S_LandCount) = 1
SelectionMode = 0
Settings_LibraryTarget = TemplateLibrary


'Create Directory Structure if not already existing
SettingsGenerate 0
If DirExists(LibraryDirectory) = 0 Then
    DebugPrint "Creating Empty Library Folder."
    MkDir LibraryDirectory 'create parent
End If
If DirExists(LibraryDirectory + Settings_LibraryTarget) = 0 Then
    DebugPrint "Generating Template Library at: " + LibraryDirectory + Settings_LibraryTarget
    MkDir LibraryDirectory + Settings_LibraryTarget
    MkDir LibraryDirectory + Settings_LibraryTarget + CommonDirectory
    MkDir LibraryDirectory + Settings_LibraryTarget + UncommonDirectory
    MkDir LibraryDirectory + Settings_LibraryTarget + RareDirectory
    MkDir LibraryDirectory + Settings_LibraryTarget + MythicDirectory
    MkDir LibraryDirectory + Settings_LibraryTarget + BonusDirectory
    MkDir LibraryDirectory + Settings_LibraryTarget + LandDirectory
    Open LibraryDirectory + Settings_LibraryTarget + WeightsFile For Binary As #1
    Put #1, , Weights()
    Close #1
End If

If DirExists(PackDirectory) = 0 Then
    DebugPrint "Creating Empty Output Pack Folder."
    MkDir PackDirectory
End If



'load settings and weights
ReloadSettings:
LoadSettings
DebugPrint "Target Library Path: " + LibraryDirectory + Settings_LibraryTarget
DebugPrint "Loading Target Library Weights."
Open LibraryDirectory + Settings_LibraryTarget + WeightsFile For Binary As #1
Get #1, , Weights()
Close #1

'build library array
DebugPrint "Building Library Array From Target Library:"
LibArrayMaxSize = 0

'grab total number of cards in library
For i = 0 To Wsize
    'prompt file string
    FilesPrompt i
    'count cards
    RarityCount = 0
    Do While DummyString <> ""
        If DummyString <> "./" And DummyString <> "../" And DummyString <> ".\" And DummyString <> "..\" And DummyString <> "" Then RarityCount = RarityCount + 1
        DummyString = Files$
    Loop
    LibraryMaxSize(i) = RarityCount
    If RarityCount > LibArrayMaxSize Then LibArrayMaxSize = RarityCount
    LibraryTotalSize = LibraryTotalSize + RarityCount
Next
DebugPrint "Largest Rarity Size: " + Str$(LibArrayMaxSize)
DebugPrint "Total Library Size Detected:" + Str$(LibraryTotalSize)
For i = 0 To Wsize
    DebugPrint Str$(LibraryMaxSize(i))
Next



'build library array
Dim LibraryArray(Wsize, LibArrayMaxSize) As String 'this is THE array that hold all of the file names for the fun little cards
For i = 0 To Wsize
    FilesPrompt i
    For ii = 0 To LibraryMaxSize(i)
        LibraryArray(i, ii) = Files$
        If LibraryArray(i, ii) <> "../" And LibraryArray(i, ii) <> "..\" Then DebugPrint "Found File: " + LibraryArray(i, ii)
    Next
Next
'BUG TURNED INTO FEATURE CAUSE LAZY
'LibraryArray(X,0) should always return "../" or "..\" os depending. from this point on we should only target (X,1) and higher for any actual use


DebugPrint "Detecting Additional Libraries:"
Dim LibraryCount
Dim RecursionCounter
ReDim AddtLibraryPathArray(LibraryCount) As String 'some bullshit that i have to define the array here just so the compiler is happy.
'Dim LibraryLableDummy As String
RecursionCounter = 0
i = 0
RePass:
DummyString = Files$(LibraryDirectory)
Do While DummyString <> ""
    If DummyString <> "./" And DummyString <> "../" And DummyString <> ".\" And DummyString <> "..\" And DummyString <> "" Then
        If RecursionCounter <> 0 Then DebugPrint "Probing " + LibraryDirectory + DummyString + WeightsFile
        If FileExists(LibraryDirectory + DummyString + WeightsFile) <> 0 Then
            If RecursionCounter <> 0 Then DebugPrint "Possible Valid Library Detected: " + DummyString
            If RecursionCounter = 0 Then LibraryCount = LibraryCount + 1
            If RecursionCounter = 1 Then
                i = i + 1
                AddtLibraryPathArray(i) = DummyString
            End If
        End If
    End If
    DummyString = Files$
Loop

If RecursionCounter = 0 Then
    ReDim AddtLibraryPathArray(LibraryCount) As String
    RecursionCounter = 1
    GoTo RePass
End If
RecursionCounter = 0





DebugPrint "..."
DebugPrint "Loading Complete."
DebugPrint ""

Dim MenuKey As Long
Dim MenuItem
Dim MenuConf
Dim LibraryInc

Delay 0.5
MainMenu:
KeyClear
Do

    Cls
    Locate 1, 1
    Print "Maxaroth's Game Pack Maker for Reena (MGPMR)"
    Print VersionString;
    Color RGB(255, 0, 0): Print " " + ErrorReturn;: Color RGB(255, 255, 255): Print
    Locate 3, 1: Print
    Color RGB(0, 255, 0): Print "  Generate Packs": Color RGB(255, 255, 255)
    Print
    Select Case SelectionMode
        Case 0
            Print "Library Weights:"
            Print "  Land Weight: ";: ColorNumberPrint (Trim$(Str$(Weights(R_Land)))), 1: Print " (Cards Detected: ";: ColorNumberPrint Trim$(Str$(LibraryMaxSize(R_Land))), 0: Print ")"
            Print "  Common Weight: ";: ColorNumberPrint Trim$(Str$(Weights(R_common))), 1: Print " (Cards Detected: ";: ColorNumberPrint Trim$(Str$(LibraryMaxSize(R_common))), 0: Print ")"
            Print "  Uncommon Weight: ";: ColorNumberPrint Trim$(Str$(Weights(R_uncommon))), 1: Print " (Cards Detected: ";: ColorNumberPrint Trim$(Str$(LibraryMaxSize(R_uncommon))), 0: Print ")"
            Print "  Rare Weight: ";: ColorNumberPrint Trim$(Str$(Weights(R_rare))), 1: Print " (Cards Detected: ";: ColorNumberPrint Trim$(Str$(LibraryMaxSize(R_rare))), 0: Print ")"
            Print "  Mythic Weight: ";: ColorNumberPrint Trim$(Str$(Weights(R_mythic))), 1: Print " (Cards Detected: ";: ColorNumberPrint Trim$(Str$(LibraryMaxSize(R_mythic))), 0: Print ")"
            Print "  Bonus Weight: ";: ColorNumberPrint Trim$(Str$(Weights(R_bonus))), 1: Print " (Cards Detected: ";: ColorNumberPrint Trim$(Str$(LibraryMaxSize(R_bonus))), 0: Print ")"
        Case 1
            Print "Number of Cards to Include:"
            Print "  Land Cards: ";: ColorNumberPrint (Trim$(Str$(Weights(R_Land)))), 1: Print " (Cards Detected: ";: ColorNumberPrint Trim$(Str$(LibraryMaxSize(R_Land))), 0: Print ")"
            Print "  Common Cards: ";: ColorNumberPrint Trim$(Str$(Weights(R_common))), 1: Print " (Cards Detected: ";: ColorNumberPrint Trim$(Str$(LibraryMaxSize(R_common))), 0: Print ")"
            Print "  Uncommon Cards: ";: ColorNumberPrint Trim$(Str$(Weights(R_uncommon))), 1: Print " (Cards Detected: ";: ColorNumberPrint Trim$(Str$(LibraryMaxSize(R_uncommon))), 0: Print ")"
            Print "  Rare Cards: ";: ColorNumberPrint Trim$(Str$(Weights(R_rare))), 1: Print " (Cards Detected: ";: ColorNumberPrint Trim$(Str$(LibraryMaxSize(R_rare))), 0: Print ")"
            Print "  Mythic Cards: ";: ColorNumberPrint Trim$(Str$(Weights(R_mythic))), 1: Print " (Cards Detected: ";: ColorNumberPrint Trim$(Str$(LibraryMaxSize(R_mythic))), 0: Print ")"
            Print "  Bonus Cards: ";: ColorNumberPrint Trim$(Str$(Weights(R_bonus))), 1: Print " (Cards Detected: ";: ColorNumberPrint Trim$(Str$(LibraryMaxSize(R_bonus))), 0: Print ")"
    End Select
    Print
    Print "Program Settings:"
    If SelectionMode = 1 Then
        Color RGB(100, 100, 100)
        Print "  Disabled in Count mode."
    Else
        Print "  Cards per Pack: ";: ColorNumberPrint Trim$(Str$(Settings(S_PackSize))), 2: Print
    End If
    Color RGB(255, 255, 255)
    Print "  Packs to Generate: ";: ColorNumberPrint Trim$(Str$(Settings(S_PackCount))), 0: Print
    If SelectionMode = 1 Then
        Color RGB(100, 100, 100)
        Print "  Disabled in Count mode."
    Else
        Print "  Guarenteed Lands per Pack: ";: ColorNumberPrint Trim$(Str$(Settings(S_LandCount))), 0: Print
    End If
    Color RGB(255, 255, 255)

    If MenuItem <> 10 Then Print "  Active Library: " + Chr$(34) + LibraryDirectory + Settings_LibraryTarget + Chr$(34) + " (Total Libraries Detected: ";: ColorNumberPrint Trim$(Str$(LibraryCount)), 0: Print ")"
    If MenuItem = 10 Then Color RGB(0, 255, 0): Print "  Select Library: ";: Print Chr$(34) + LibraryDirectory + AddtLibraryPathArray(LibraryInc) + Chr$(34);: Color RGB(255, 255, 255): Print " (Total Libraries Detected: ";: ColorNumberPrint Trim$(Str$(LibraryCount)), 0: Print ")"


    Print "  Selection Mode: ";
    If SelectionMode = 0 Then Print "Weight";
    If SelectionMode = 1 Then Print "Count";
    Print ""

    Print
    If MenuItem = 12 Then Color RGB(255, 0, 0)
    Print "  Exit Program"
    Color RGB(255, 255, 255)

    'these following print statements should have an ascii arrow pointing right. ensure it is present when building
    If MenuItem = 0 Then Locate 4, 1: Print "";
    If MenuItem = 1 Then Locate 7, 1: Print "";
    If MenuItem = 2 Then Locate 8, 1: Print "";
    If MenuItem = 3 Then Locate 9, 1: Print "";
    If MenuItem = 4 Then Locate 10, 1: Print "";
    If MenuItem = 5 Then Locate 11, 1: Print "";
    If MenuItem = 6 Then Locate 12, 1: Print "";
    If MenuItem = 7 Then Locate 15, 1: Print "";
    If MenuItem = 8 Then Locate 16, 1: Print "";
    If MenuItem = 9 Then Locate 17, 1: Print "";
    If MenuItem = 10 Then Locate 18, 1: Print "";
    If MenuItem = 11 Then Locate 19, 1: Print "";
    If MenuItem = 12 Then Locate 21, 1: Print "";


    If MenuItem > 12 Then MenuItem = 0
    If MenuItem < 0 Then MenuItem = 12


    MenuKey = KeyHit
    If MenuKey = -20480 Or MenuKey = -115 Or MenuKey = -83 Then MenuItem = MenuItem + 1
    If MenuKey = -18432 Or MenuKey = -119 Or MenuKey = -87 Then MenuItem = MenuItem - 1
    If MenuKey = -13 Or MenuKey = -32 Then MenuConf = 1


    If MenuItem > 0 And MenuItem < 7 Then 'weights counters
        If MenuKey = -19200 Or MenuKey = -97 Or MenuKey = -65 Then
            If Abs(Shift) <> SelectionMode Then
                Weights(MenuItem - 1) = Weights(MenuItem - 1) - 1
            Else
                Weights(MenuItem - 1) = Weights(MenuItem - 1) - 5
            End If
        End If
        If MenuKey = -19712 Or MenuKey = -100 Or MenuKey = -68 Then
            If Abs(Shift) <> SelectionMode Then
                Weights(MenuItem - 1) = Weights(MenuItem - 1) + 1
            Else
                Weights(MenuItem - 1) = Weights(MenuItem - 1) + 5
            End If
        End If
        If Weights(MenuItem - 1) < 0 Then Weights(MenuItem - 1) = 0

    End If

    If MenuItem > 6 And MenuItem < 10 Then 'settings counters
        If MenuKey = -19200 Or MenuKey = -97 Or MenuKey = -65 Then Settings(MenuItem - 7) = Settings(MenuItem - 7) - 1
        If MenuKey = -19712 Or MenuKey = -100 Or MenuKey = -68 Then Settings(MenuItem - 7) = Settings(MenuItem - 7) + 1
        If Settings(MenuItem - 7) < 0 Then Settings(MenuItem - 7) = 0
    End If

    If MenuItem = 10 Then 'library select
        If MenuKey = -19200 Or MenuKey = -97 Or MenuKey = -65 Then LibraryInc = LibraryInc - 1
        If MenuKey = -19712 Or MenuKey = -100 Or MenuKey = -68 Then LibraryInc = LibraryInc + 1
        If LibraryInc < 1 Then LibraryInc = 1
        If LibraryInc > LibraryCount Then LibraryInc = LibraryCount
    End If



    If MenuConf = 1 Then
        'incase we have to come back, reset necessary variables now
        MenuConf = 0
        'what to we do when enter is hit?
        Select Case MenuItem
            Case 0
                GoTo generate
            Case 1 To 9
                'prompt for manual value input
            Case 10
                Settings_LibraryTarget = AddtLibraryPathArray(LibraryInc)
                SaveSettings
                Run
            Case 11
                If SelectionMode = 1 Then Run
                SelectionMode = 1
                For i = 0 To Wsize
                    Weights(i) = 0
                Next
                DebugPrint "Changing to Card Count mode!  Weights have been overwritten in memory and saving disabled."
            Case 12
                System
        End Select
    End If



    Display
    Limit 30
Loop

generate:
AutoDisplay

'save settings
SaveSettings
If SelectionMode = 0 Then
    Open LibraryDirectory + Settings_LibraryTarget + WeightsFile For Binary As #1
    Put #1, , Weights()
    Close #1
End If

'Check if cards exist
ii = 0
For i = 0 To Wsize
    ii = ii + LibraryMaxSize(i)
Next
If ii = 0 Then Error 100

'check if weight configuration is valid
For i = 0 To Wsize
    If LibraryMaxSize(i) = 0 And Weights(i) > 0 Then Error 101
Next

If SelectionMode = 0 Then
    'check if card count is valid
    If Settings(S_PackSize) > LibraryTotalSize Then Error 102

    'check if forcing lands out of an empty land folder
    If Settings(S_LandCount) > LibraryMaxSize(R_Land) Then Error 105
End If
If SelectionMode = 1 Then
    'check if overdrawing
    For i = 0 To Wsize
        If Weights(i) > LibraryMaxSize(i) Then Error 106
    Next
End If

If SelectionMode = 0 Then DebugPrint "Generating" + Str$(Settings(S_PackCount)) + " Packs containing" + Str$(Settings(S_PackSize)) + " Cards."
If SelectionMode = 1 Then DebugPrint "Generating" + Str$(Settings(S_PackCount)) + " Packs containing set card counts."
Dim PackIterative As Single
Dim CardIterative As Single
Dim WinningCard As String
Dim WinningRarity As Single
Dim CumulativeWeight
Dim TargetWeight
Dim IterativeWeight

'get weight range
For i = 0 To Wsize
    CumulativeWeight = CumulativeWeight + Weights(i)
Next


MkDir PackDirectory + SessionString + "\"
For PackIterative = 1 To Settings(S_PackCount)
    MkDir PackDirectory + SessionString + "\" + "Pack" + Trim$(Str$(PackIterative))
    Select Case SelectionMode
        Case 0

            For CardIterative = 1 To Settings(S_PackSize)
                DebugPrint "Pack " + Trim$(Str$(PackIterative))
                DebugPrint "Card " + Trim$(Str$(CardIterative))
                DebugPrint "Forced Lands " + Trim$(Str$(Settings(S_LandCount)))
                If CardIterative <= Settings(S_LandCount) Then 'force X lands in each pack
                    WinningRarity = R_Land
                    WinningCard = LibraryArray(WinningRarity, 1 + Int(Rnd * LibraryMaxSize(WinningRarity)))
                Else 'fill rest of pack


                    'find target
                    TargetWeight = Rnd * CumulativeWeight

                    'iterate to target threshold
                    IterativeWeight = 0
                    For i = 0 To Wsize
                        IterativeWeight = IterativeWeight + Weights(i)
                        If IterativeWeight > TargetWeight Then
                            WinningRarity = i
                            Exit For
                        End If
                    Next

                    WinningCard = LibraryArray(WinningRarity, 1 + Int(Rnd * LibraryMaxSize(WinningRarity)))
                End If
                DebugPrint "Wining Card: " + WinningCard + " at rarity " + Trim$(Str$(WinningRarity))
                'check if card already exists and reroll if it does
                If FileExists(PackDirectory + SessionString + "\" + "Pack" + Trim$(Str$(PackIterative)) + "\" + WinningCard) = -1 Then
                    CardIterative = CardIterative - 1
                End If
                'copy winning card to pack file
                Select Case HostOS
                    Case "Linux", "Mac OS"
                        Shell Hide "cp " + Chr$(34) + RarityDirectory$(WinningRarity) + WinningCard + Chr$(34) + " " + Chr$(34) + PackDirectory + SessionString + "/" + "Pack" + Trim$(Str$(PackIterative)) + "/" + WinningCard + Chr$(34)
                    Case "Windows"
                        Shell Hide "copy " + Chr$(34) + RarityDirectory$(WinningRarity) + WinningCard + Chr$(34) + " " + Chr$(34) + PackDirectory + SessionString + "\" + "Pack" + Trim$(Str$(PackIterative)) + "\" + WinningCard + Chr$(34)
                End Select
            Next
        Case 1
            For i = 0 To Wsize
                For ii = 1 To Weights(i)
                    If Weights(i) > 0 Then
                        WinningCard = LibraryArray(i, 1 + Int(Rnd * LibraryMaxSize(i)))
                        DebugPrint "Wining Card: " + WinningCard + " at rarity " + Trim$(Str$(i))
                        'check if card already exists and reroll if it does
                        If FileExists(PackDirectory + SessionString + "\" + "Pack" + Trim$(Str$(PackIterative)) + "\" + WinningCard) = -1 Then
                            ii = ii - 1
                        End If
                        'copy winning card to pack file
                        Select Case HostOS
                            Case "Linux", "Mac OS"
                                Shell Hide "cp " + Chr$(34) + RarityDirectory$(i) + WinningCard + Chr$(34) + " " + Chr$(34) + PackDirectory + SessionString + "/" + "Pack" + Trim$(Str$(PackIterative)) + "/" + WinningCard + Chr$(34)
                            Case "Windows"
                                Shell Hide "copy " + Chr$(34) + RarityDirectory$(i) + WinningCard + Chr$(34) + " " + Chr$(34) + PackDirectory + SessionString + "\" + "Pack" + Trim$(Str$(PackIterative)) + "\" + WinningCard + Chr$(34)
                        End Select
                    End If
                Next
            Next

    End Select
Next

System


ErrorHandler:
Const MainMenu = 1
Const Terminate = 2
Const RegenerateSettings = 3
Select Case HandlerRoutine
    Case MainMenu
        Resume MainMenu
    Case Terminate
        System
    Case RegenerateSettings
        SettingsGenerate 1
        Resume ReloadSettings
    Case Else 'and 0
        Resume Next
End Select

Function Shift
    If KeyDown(100303) Or KeyDown(100304) Then Shift = -1
End Function

Function HandlerRoutine
    Dim CapturedError
    Dim CapturedLine
    Dim CapturedType As String
    CapturedError = Err
    CapturedLine = ErrorLine
    Select Case CapturedError
        Case 100 'attempted to generate pack(s) with an empty library
            'resolution: assume user is generating template library and ~~close MGPMR~~ return to main menu to show error message
            ErrorReturn = "No cards detected!  Please select a non empty library."
            HandlerRoutine = MainMenu
        Case 101 'attempted to generate pack(s) with a weight value greater than 0 targeting an empty folder
            'resolution: return to the main menu with an error return message
            If SelectionMode = 0 Then ErrorReturn = "Invalid configuration detected!  Please set empty folder weights to 0."
            If SelectionMode = 1 Then ErrorReturn = "Invalid configuration detected!  Please set empty folder counts to 0."
            HandlerRoutine = MainMenu
        Case 102 'attempted to generate packs with more cards than in library
            ErrorReturn = "Invalid configuration detected!  Too many cards per pack."
            HandlerRoutine = MainMenu
        Case 103 'settings file is from a different operating system
            ErrorReturn = "Loaded settings file was generated on a different OS!  Auto Regenerating."
            HandlerRoutine = RegenerateSettings
        Case 104 'settings file is from a different version
            ErrorReturn = "Loaded settings file was generated on a different version!  Auto Regenerating."
            HandlerRoutine = RegenerateSettings
        Case 105 'attempted to force pick more land than exist
            ErrorReturn = "Invalid configuration detected!  Too many lands guarenteed."
            HandlerRoutine = MainMenu
        Case 106
            ErrorReturn = "Invalid configuration detected!  Overdrawing rarity."
        Case Else 'gracefully present the user with the raw error code if not otherwise handled
            ErrorReturn = "Generic Error Detected!  Error code: " + Trim$(Str$(CapturedError)) + "@" + Trim$(Str$(CapturedLine))
            HandlerRoutine = MainMenu
    End Select
    'return a m
    DebugPrint "-----------------------------------"
    DebugPrint "MGPMR ERROR HANDLER: " + Trim$(Str$(CapturedError)) + "@" + Trim$(Str$(CapturedLine))
    DebugPrint "Error Number: " + Trim$(Str$(CapturedError))
    DebugPrint "Error Line: " + Trim$(Str$(CapturedLine))
    If CapturedError >= 100 And CapturedError <= 150 Then CapturedType = "MGPMR Error" Else CapturedType = "QB64 Error"
    DebugPrint "Error Type: " + CapturedType
    DebugPrint "Error Return Message: " + ErrorReturn
    DebugPrint "-----------------------------------"
End Function

Sub SettingsGenerate (Forced)
    Dim SaveableVariable As String
    SaveableVariable = VersionString
    If DirExists(SettingsDirectory) = 0 Or Forced = 1 Then
        If Forced = 1 Then
            DebugPrint "Deleting Existing Settings Folder."
            Kill SettingsDirectory + SettingsFile
            RmDir SettingsDirectory
        End If
        DebugPrint "Generating Settings Folder."
        MkDir SettingsDirectory
        Open SettingsDirectory + SettingsFile For Random As #1
        Put #1, 1, HostOS
        Put #1, 2, SaveableVariable
        Put #1, 3, Settings()
        Put #1, 4, Settings_LibraryTarget
        Close #1
    End If
End Sub

Sub SaveSettings
    Open SettingsDirectory + SettingsFile For Random As #1
    Put #1, 3, Settings()
    Put #1, 4, Settings_LibraryTarget
    Close #1
End Sub

Sub LoadSettings
    Dim FileOS As String
    Dim FileVer As String
    DebugPrint "Loading Settings."
    Open SettingsDirectory + SettingsFile For Random As #1
    Get #1, 1, FileOS 'Operating system
    Get #1, 2, FileVer 'Version number
    DebugPrint "File OS: " + Chr$(34) + FileOS + Chr$(34) + " | Host OS: " + Chr$(34) + HostOS + Chr$(34)
    DebugPrint "File Version: " + Chr$(34) + FileVer + Chr$(34) + " | Client Version: " + Chr$(34) + VersionString + Chr$(34)
    If FileOS <> HostOS Then Close #1: Error 103
    If FileVer <> VersionString Then Close #1: Error 104
    Get #1, 3, Settings()
    Get #1, 4, Settings_LibraryTarget
    Close #1
End Sub


Sub ColorNumberPrint (Value$, Mode)
    Dim OrangeOffset
    OrangeOffset = 150 - (Val(Value$) * 2)
    If OrangeOffset < 100 Then OrangeOffset = 100
    If Mode = 1 Then Color RGB(255 - Val(Value$) * 2.3, 255 - OrangeOffset, 0) 'weight color gradiant
    If Mode = 1 And SelectionMode = 1 Then Color RGB(0, 155, 0) 'count mode green
    If Val(Value$) = 0 Then Color RGB(255, 0, 0) '0 shall be red
    If Mode = 2 And Val(Value$) > LibraryTotalSize Then Color RGB(255, 0, 0)
    Print Value$;
    Color RGB(255, 255, 255)
End Sub

Function RarityDirectory$ (Rarity)
    Select Case Rarity
        Case R_Land
            RarityDirectory$ = LibraryDirectory + Settings_LibraryTarget + LandDirectory
        Case R_common
            RarityDirectory$ = LibraryDirectory + Settings_LibraryTarget + CommonDirectory
        Case R_uncommon
            RarityDirectory$ = LibraryDirectory + Settings_LibraryTarget + UncommonDirectory
        Case R_rare
            RarityDirectory$ = LibraryDirectory + Settings_LibraryTarget + RareDirectory
        Case R_mythic
            RarityDirectory$ = LibraryDirectory + Settings_LibraryTarget + MythicDirectory
        Case R_bonus
            RarityDirectory$ = LibraryDirectory + Settings_LibraryTarget + BonusDirectory

    End Select
End Function



Sub FilesPrompt (Rarity)
    DummyString = Files$(RarityDirectory$(Rarity))
End Sub

Sub DebugPrint (Message$)
    If DebugMode = 1 Then

        Echo Message$
    End If
End Sub


Sub OSPROBE
    HostOS = "Linux" 'if detection fails, assume POSIX compliant system, all linux and mac os targeted code should be POSIX compliant so this is the best default case.
    If InStr(OS$, "WINDOWS") Then HostOS = "Windows"
    If InStr(OS$, "LINUX") Then HostOS = "Linux"
    If InStr(OS$, "MACOSX") Then HostOS = "Mac OS"
End Sub

