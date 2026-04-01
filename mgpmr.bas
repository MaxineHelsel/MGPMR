$NoPrefix
Option Explicit
Randomize Timer
Dim Shared HostOS As String
OSPROBE

Const DebugMode = 0

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

$End If

'Global Variables
Dim SessionString As String
Select Case HostOS
    Case "Mac OS", "Linux"
        SessionString = Date$ + "_" + Time$
    Case "Windows"
        SessionString = Date$ + "_" + LTrim$(RTrim$(Str$(Int(Timer))))
End Select

Const Wsize = 5
Const R_Land = 0
Const R_common = 1
Const R_uncommon = 2
Const R_rare = 3
Const R_mythic = 4
Const R_bonus = 5

Dim Weights(Wsize) As Single

Const Ssize = 3
Const S_PackSize = 0
Const S_PackCount = 1
Const S_LandCount = 2

Dim Settings(Ssize) As Single

'default weights
Weights(R_Land) = 0
Weights(R_common) = 100
Weights(R_uncommon) = 50
Weights(R_rare) = 25
Weights(R_mythic) = 10
Weights(R_bonus) = 0

Settings(S_PackSize) = 10
Settings(S_PackCount) = 10
Settings(S_LandCount) = 1

'Create Directory Structure if not already existing
If DirExists(LibraryDirectory) = 0 Then
    DebugPrint "Creating Empty Library Folder."
    MkDir LibraryDirectory 'create parent
    MkDir LibraryDirectory + CommonDirectory
    MkDir LibraryDirectory + UncommonDirectory
    MkDir LibraryDirectory + RareDirectory
    MkDir LibraryDirectory + MythicDirectory
    MkDir LibraryDirectory + BonusDirectory
    MkDir LibraryDirectory + LandDirectory
    Open LibraryDirectory + WeightsFile For Binary As #1
    Put #1, , Weights()
    Close #1
End If
If DirExists(PackDirectory) = 0 Then
    DebugPrint "Creating Empty Output Pack Folder."
    MkDir PackDirectory
End If
If DirExists(SettingsDirectory) = 0 Then
    DebugPrint "Creating Empty Settings Folder"
    MkDir SettingsDirectory
    Open SettingsDirectory + SettingsFile For Binary As #1
    Put #1, , Settings()
    Close #1
End If

'load settings and weights
DebugPrint "Loading Library Weights."
Open LibraryDirectory + WeightsFile For Binary As #1
Get #1, , Weights()
Close #1
DebugPrint "Loading Settings."
Open SettingsDirectory + SettingsFile For Binary As #1
Get #1, , Settings()
Close #1

DebugPrint "Building Library Array:"
'GoTo fuck:
Dim Shared DummyString As String
Dim RarityCount As Single
Dim LibraryMaxSize(Wsize) As Single
Dim LibArrayMaxSize As Single
Dim i, ii

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
Next
DebugPrint "Largest Folder: " + Str$(LibArrayMaxSize)


'build library array
Dim LibraryArray(Wsize, LibArrayMaxSize) As String 'this is THE array that hold all of the file names for the fun little cards
For i = 0 To Wsize
    FilesPrompt i
    For ii = 0 To LibraryMaxSize(i)
        LibraryArray(i, ii) = Files$
        'debugPrint "Found File: " + LibraryArray(i, ii)
    Next
Next
'BUG TURNED INTO FEATURE CAUSE LAZY
'LibraryArray(X,0) should always return "../" or "..\" os depending. from this point on we should only target (X,1) and higher for any actual use

'debug print library size
DebugPrint "Total Library Size Detected:"
For i = 0 To Wsize
    DebugPrint Str$(LibraryMaxSize(i))
Next


DebugPrint "..."
DebugPrint "Loading Complete."
DebugPrint ""

Dim MenuKey As Long
Dim MenuItem
Dim MenuConf

Delay 0.5

Do

    Cls
    Locate 1, 1
    Print "Maxaroth's Game Pack Maker for Reena"
    Print "V1.1"
    Print
    Print "  Generate Packs"
    Print
    Print "Current Parameters:"
    Print "  Land Weight: " + Trim$(Str$(Weights(R_Land))) + " (Detected: " + Trim$(Str$(LibraryMaxSize(R_Land))) + ")"
    Print "  Common Weight: " + Trim$(Str$(Weights(R_common))) + " (Detected: " + Trim$(Str$(LibraryMaxSize(R_common))) + ")"
    Print "  Uncommon Weight: " + Trim$(Str$(Weights(R_uncommon))) + " (Detected: " + Trim$(Str$(LibraryMaxSize(R_uncommon))) + ")"
    Print "  Rare Weight: " + Trim$(Str$(Weights(R_rare))) + " (Detected: " + Trim$(Str$(LibraryMaxSize(R_rare))) + ")"
    Print "  Mythic Weight: " + Trim$(Str$(Weights(R_mythic))) + " (Detected: " + Trim$(Str$(LibraryMaxSize(R_mythic))) + ")"
    Print "  Bonus Weight: " + Trim$(Str$(Weights(R_bonus))) + " (Detected: " + Trim$(Str$(LibraryMaxSize(R_bonus))) + ")"
    Print "  Cards per Pack: " + Trim$(Str$(Settings(S_PackSize)))
    Print "  Packs to Generate: " + Trim$(Str$(Settings(S_PackCount)))
    Print "  Guarenteed Lands per Pack: " + Trim$(Str$(Settings(S_LandCount)))
    Print
    Print "  Exit Program"

    If MenuItem = 0 Then Locate 4, 1: Print "";
    If MenuItem = 1 Then Locate 7, 1: Print "";
    If MenuItem = 2 Then Locate 8, 1: Print "";
    If MenuItem = 3 Then Locate 9, 1: Print "";
    If MenuItem = 4 Then Locate 10, 1: Print "";
    If MenuItem = 5 Then Locate 11, 1: Print "";
    If MenuItem = 6 Then Locate 12, 1: Print "";
    If MenuItem = 7 Then Locate 13, 1: Print "";
    If MenuItem = 8 Then Locate 14, 1: Print "";
    If MenuItem = 9 Then Locate 15, 1: Print "";
    If MenuItem = 10 Then Locate 17, 1: Print "";

    If MenuItem > 10 Then MenuItem = 0
    If MenuItem < 0 Then MenuItem = 10


    MenuKey = KeyHit
    If MenuKey = -20480 Or MenuKey = -115 Or MenuKey = -83 Then MenuItem = MenuItem + 1
    If MenuKey = -18432 Or MenuKey = -119 Or MenuKey = -87 Then MenuItem = MenuItem - 1
    If MenuKey = -13 Or MenuKey = -32 Then MenuConf = 1

    If MenuItem > 0 And MenuItem < 7 Then
        If MenuKey = -19200 Or MenuKey = -97 Or MenuKey = -65 Then
            If KeyDown(100303) Or KeyDown(100304) Then
                Weights(MenuItem - 1) = Weights(MenuItem - 1) - 1
            Else
                Weights(MenuItem - 1) = Weights(MenuItem - 1) - 5
            End If
        End If
        If MenuKey = -19712 Or MenuKey = -100 Or MenuKey = -68 Then
            If KeyDown(100303) Or KeyDown(100304) Then
                Weights(MenuItem - 1) = Weights(MenuItem - 1) + 1
            Else
                Weights(MenuItem - 1) = Weights(MenuItem - 1) + 5
            End If
        End If
        If Weights(MenuItem - 1) < 0 Then Weights(MenuItem - 1) = 0

        'Floating Poing Fix
        Weights(MenuItem - 1) = Weights(MenuItem - 1) * 100
        Weights(MenuItem - 1) = Round(Weights(MenuItem - 1))
        Weights(MenuItem - 1) = Weights(MenuItem - 1) / 100
    End If

    If MenuItem > 6 And MenuItem < 10 Then
        If MenuKey = -19200 Or MenuKey = -97 Or MenuKey = -65 Then Settings(MenuItem - 7) = Settings(MenuItem - 7) - 1
        If MenuKey = -19712 Or MenuKey = -100 Or MenuKey = -68 Then Settings(MenuItem - 7) = Settings(MenuItem - 7) + 1
        If Settings(MenuItem - 7) < 0 Then Settings(MenuItem - 7) = 0
    End If



    If MenuConf = 1 Then
        Select Case MenuItem
            Case 0
                GoTo generate
            Case 1 To 9
                'prompt for manual value input
            Case 10
                System
        End Select
    End If



    Display
    Limit 60
Loop

generate:
AutoDisplay

'save settings
Open LibraryDirectory + WeightsFile For Binary As #1
Put #1, , Weights()
Close #1
Open SettingsDirectory + SettingsFile For Binary As #1
Put #1, , Settings()
Close #1




DebugPrint "Generating" + Str$(Settings(S_PackCount)) + " Packs containing" + Str$(Settings(S_PackSize)) + " Cards."

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
    For CardIterative = 1 To Settings(S_PackSize)
        DebugPrint Str$(CardIterative)
        DebugPrint Str$(Settings(S_LandCount))
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

            'OLD DIRTY METHOD
            'Select EveryCase Rnd
            '    Case Is < Weights(R_Land)
            '        WinningRarity = R_Land
            '    Case Is < Weights(R_common)
            '        WinningRarity = R_common
            '    Case Is < Weights(R_uncommon)
            '        WinningRarity = R_uncommon
            '    Case Is < Weights(R_rare)
            '        WinningRarity = R_rare
            '    Case Is < Weights(R_mythic)
            '        WinningRarity = R_mythic
            '    Case Is < Weights(R_bonus)
            '        WinningRarity = R_bonus
            'End Select


            WinningCard = LibraryArray(WinningRarity, 1 + Int(Rnd * LibraryMaxSize(WinningRarity)))
        End If
        DebugPrint WinningCard
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
Next

System

Function RarityDirectory$ (Rarity)
    Select Case Rarity
        Case R_Land
            RarityDirectory$ = LibraryDirectory + LandDirectory
        Case R_common
            RarityDirectory$ = LibraryDirectory + CommonDirectory
        Case R_uncommon
            RarityDirectory$ = LibraryDirectory + UncommonDirectory
        Case R_rare
            RarityDirectory$ = LibraryDirectory + RareDirectory
        Case R_mythic
            RarityDirectory$ = LibraryDirectory + MythicDirectory
        Case R_bonus
            RarityDirectory$ = LibraryDirectory + BonusDirectory

    End Select
End Function



Sub FilesPrompt (Rarity)
    Select Case Rarity
        Case R_Land
            DummyString = Files$(LibraryDirectory + LandDirectory)
        Case R_common
            DummyString = Files$(LibraryDirectory + CommonDirectory)
        Case R_uncommon
            DummyString = Files$(LibraryDirectory + UncommonDirectory)
        Case R_rare
            DummyString = Files$(LibraryDirectory + RareDirectory)
        Case R_mythic
            DummyString = Files$(LibraryDirectory + MythicDirectory)
        Case R_bonus
            DummyString = Files$(LibraryDirectory + BonusDirectory)
    End Select


End Sub

Sub DebugPrint (Message$)

    If DebugMode = 1 Then Print Message$

End Sub

Sub OSPROBE
    HostOS = "Unknown OS"
    If InStr(OS$, "WINDOWS") Then HostOS = "Windows"
    If InStr(OS$, "LINUX") Then HostOS = "Linux"
    If InStr(OS$, "MACOSX") Then HostOS = "Mac OS"
End Sub

