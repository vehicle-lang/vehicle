-- -- | Find the files matching the file patterns in a test specification 'produces' field.
-- testSpecProducesGlobDir :: FilePath -> TestSpec -> IO [FilePath]
-- testSpecProducesGlobDir testDirectory testSpec =
--   FilePattern.glob (testSpecProduces testSpec) testDirectory

-- -- | Compare two test outputs using the options set in Ignore.
-- testSpecIgnoreTestOutput :: TestSpec -> TestOutput -> TestOutput -> IO (Maybe String)
-- testSpecIgnoreTestOutput testSpec golden actual = do
--   let goldenFiles = HashMap.keysSet (testOutputFiles golden)
--   let actualFiles = HashMap.keysSet (testOutputFiles actual)
--   -- Compute missing files:
--   let missingOutputFileErrors =
--         [ printf "Missing output file %s" missingFile
--           | missingFile <- sort $ HashSet.toList $ HashSet.difference goldenFiles actualFiles
--         ]
--   -- Compute extraneous files:
--   let extraOutputFileErrors =
--         [ printf "Extraneous output file %s" extraFile
--           | extraFile <- sort $ HashSet.toList $ HashSet.difference actualFiles goldenFiles
--         ]

--   -- Compare output & error stream content:
--   let differentStdoutError =
--         printf "Contents of stdout differ:\n%s"
--           <$> testSpecDiffText testSpec (testOutputStdout golden) (testOutputStdout actual)
--   let differentStderrError =
--         printf "Contents of stderr differ:\n%s"
--           <$> testSpecDiffText testSpec (testOutputStderr golden) (testOutputStderr actual)

--   -- Compare file content:
--   let sharedFiles = sort $ HashSet.toList $ HashSet.intersection goldenFiles actualFiles
--   let differentOutputFileErrors =
--         catMaybes
--           [ printf "Content of %s differs:\n%s" file
--               <$> testSpecDiffText
--                 testSpec
--                 (testOutputFiles golden HashMap.! file)
--                 (testOutputFiles actual HashMap.! file)
--             | file <- sharedFiles
--           ]
--   -- Combine all messages:
--   let messages =
--         join
--           [ maybeToList differentStdoutError,
--             maybeToList differentStderrError,
--             differentOutputFileErrors,
--             missingOutputFileErrors,
--             extraOutputFileErrors
--           ]
--   return $ boolToMaybe (not $ null messages) (unlines messages)

-- -- | Compare two texts using the options set in Ignore.
-- testSpecDiffText :: TestSpec -> Text -> Text -> Maybe String
-- testSpecDiffText testSpec golden actual = do
--   let compareLine = maybe (==) Ignore.matchLine (testSpecIgnore testSpec)
--   let goldenLines = Text.lines golden
--   let actualLines = Text.lines actual
--   let linesEqual =
--         length goldenLines == length actualLines
--           && and (zipWith compareLine goldenLines actualLines)
--   let lineTotal = length goldenLines + length actualLines
--   if linesEqual
--     then Nothing
--     else
--       if lineTotal > 20000
--         then
--           Just $
--             "<too big to diff (" <> show (length goldenLines) <> "lines vs " <> show (length actualLines) <> " lines>"
--         else do
--           let diffGroups = getGroupedDiffBy compareLine goldenLines actualLines

--           -- ASSERT: we should not be computing the pretty diff unless
--           -- there is an actual difference, guarded by the comparison
--           let prettyDiff =
--                 assert (not (all isBoth diffGroups)) $
--                   ppDiff (mapDiff (fmap Text.unpack) <$> diffGroups)

--           return prettyDiff
--   where
--     -- TODO: upstream DiffOutput to work with Text
--     isBoth :: Diff a -> Bool
--     isBoth (Both _ _) = True
--     isBoth _ = False

--     mapDiff :: (a -> b) -> Diff a -> Diff b
--     mapDiff f (First x) = First (f x)
--     mapDiff f (Second y) = Second (f y)
--     mapDiff f (Both x y) = Both (f x) (f y)

-- -- Reading and writing .golden files:

-- -- | Read a golden file, if it exists.
-- readGoldenFile :: FilePath -> IO Text
-- readGoldenFile goldenFile = do
--   goldenFileExists <- doesFileExist goldenFile
--   if goldenFileExists then Text.readFile goldenFile else return ""

-- goldenStdoutFileName, goldenStderrFileName :: TestSpec -> FilePath
-- goldenStdoutFileName testSpec = testSpecName testSpec <.> "out" <.> "golden"
-- goldenStderrFileName testSpec = testSpecName testSpec <.> "err" <.> "golden"

-- -- | Read the golden files for a test specification.
-- readGoldenFiles :: FilePath -> TestSpec -> IO TestOutput
-- readGoldenFiles testDirectory testSpec = do
--   testOutputStdout <- readGoldenFile $ testDirectory </> goldenStdoutFileName testSpec
--   testOutputStderr <- readGoldenFile $ testDirectory </> goldenStderrFileName testSpec
--   testOutputFiles <-
--     fmap HashMap.fromList $ do
--       goldenFiles <- testSpecProducesGlobDir testDirectory testSpec
--       forM goldenFiles $ \goldenFile -> do
--         goldenFileContents <- readGoldenFile goldenFile
--         -- let relativeFilePath = makeRelative testDirectory $ goldenFile -<.> "golden"
--         let relativeFilePath = makeRelative testDirectory goldenFile
--         -- Convert the golden file path back to the corresponding output file path.
--         let outputFilePath = goldenFileToOutputFile relativeFilePath

--         return (outputFilePath, goldenFileContents)
--   return TestOutput {..}

-- goldenFileToOutputFile :: FilePath -> FilePath
-- goldenFileToOutputFile = dropExtension

-- writeGoldenFiles :: FilePath -> TestSpec -> TestOutput -> IO ()
-- writeGoldenFiles testDirectory testSpec testOutput@TestOutput {..} = do
--   validateTestSpecProduces testSpec testOutput
--   writeFileChanged (testDirectory </> goldenStdoutFileName testSpec) testOutputStdout
--   writeFileChanged (testDirectory </> goldenStderrFileName testSpec) testOutputStderr
--   for_ (HashMap.toList testOutputFiles) $ \(file, fileContents) -> do
--     writeFileChanged (testDirectory </> file <.> "golden") fileContents

-- -- | Test whether or not the 'produces' patterns still match all the produced files.
-- validateTestSpecProduces :: TestSpec -> TestOutput -> IO ()
-- validateTestSpecProduces testSpec testOutput
--   | null unmatchFileErrors = return ()
--   | otherwise = fail $ unlines unmatchFileErrors
--   where
--     isMatched filePath =
--       let goldenFilePath = filePath <.> "golden"
--        in any (`FilePattern.match` goldenFilePath) (testSpecProduces testSpec)
--     unmatchFileErrors = do
--       outputFilePath <- HashMap.keys $ testOutputFiles testOutput
--       if isMatched outputFilePath
--         then mempty
--         else return $ printf "Output file %s is not matched by file patterns in 'produces'" outputFilePath

-- Conversion from TestSpecs to JSON.
