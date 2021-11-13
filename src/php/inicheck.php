<?php

/**
 * Checks if a php.ini file needs creating or modifying and writes a new one if
 * it does, saving any original. The file(s) are written to paths passed in on
 * the command line.
 *
 * Returns a single line of output that contains info required by the setup. The
 * line must start with PHP_CHECK_ID (which is the same value as the define in
 * the main install script), followed by pipe (|) separated values and ending
 * with an eol. Note that the last value MUST not be an empty string.
 *
 * The values required are 0 or 1, signifying whether any changes have been
 * made, and an informational status message, which may contain an error.
 */

$PHP_CHECK_ID = '<ComposerSetup:>';

$ini = new IniChecker($argv);
$result = (int) $ini->needsModification();

$data = array($result, $ini->status);
printf('%s%s%s', $PHP_CHECK_ID, implode('|', $data), PHP_EOL);

exit(0);


class IniChecker
{
    public $status;
    private $phpDir;
    private $srcIni;
    private $modIni;
    private $origIni;
    private $content;
    private $iniItems;
    private $eol;
    private $changes;

    /**
     * Constructor
     *
     * The args passed in contain:
     *   [1] The tmp path for the modified ini
     *   [2] The tmp path for the original ini
     *
     * @param array $argv Command-line args
     */
    public function __construct(array $argv)
    {
        $this->modIni = $argv[1];
        $this->origIni = $argv[2];

        $this->changes = array();
        $this->writeError('Status message missing');
    }

    /**
     * Returns true if an ini was created or modified
     *
     * @return bool
     */
    public function needsModification()
    {
        if (!$this->init($new)) {
            return;
        }

        if (!$this->processChanges()) {
            return;
        }

        if (empty($this->changes)) {
            $this->status = 'No changes to ini file required';
            return;
        }

        if (!$this->iniBackup($new)) {
            $this->writeError('Failed to save backup ini: '.$this->origIni);
            return;
        }

        if (!$this->iniSave()) {
            $this->writeError('Unable to save modified ini: '.$this->modIni);
            return;
        }

        $changes = implode(',', $this->changes);

        if ($new) {
            $this->status = 'New ini created: '.$changes;
        } else {
            $this->status = 'Changes made: '.$changes;
        }

        return true;
    }

    /**
     * Returns true if various init checks succeed
     *
     * @param null|bool $new Set by method
     *
     * @return bool
     */
    private function init(&$new)
    {
        if (!defined('PHP_WINDOWS_VERSION_BUILD')) {
            $this->writeError('Non-Windows build: '.PHP_OS);
            return;
        }

        if ($missing = $this->checkBuiltIns()) {
            $this->writeError('Built-in extensions not avaliable: '.implode(',', $missing));
            return;
        }

        // We should have this, but just in case
        if (!defined('PHP_BINARY')) {
            $this->writeError('PHP_BINARY not defined');
            return;
        }

        $this->phpDir = dirname(PHP_BINARY);

        // Make sure we can write to the php directory
        if (!is_writable($this->phpDir)) {
            $this->writeError('Directory is not writable: '.$this->phpDir);
            return;
        }

        // We can only handle a single ini
        if (!$this->checkScanDirConfig() || php_ini_scanned_files()) {
            $this->writeError('Multiple php ini files are being used');
            return;
        }

        if ($this->srcIni = strval(php_ini_loaded_file())) {
            // We need the ini to be in the php directory
            if (strtolower($this->phpDir) !== strtolower(dirname($this->srcIni))) {
                $this->writeError('Loaded ini is not in php directory');
                return;
            }
        } else {
            $new = true;
            $this->srcIni = $this->phpDir.'\\php.ini-production';
        }

        if (!$this->iniRead($this->srcIni)) {
            $this->writeError('Failed to read source ini: '.$srcIni);
            return;
        }

        if (!$this->iniItems = parse_ini_string($this->content)) {
            $this->writeError('Failed to parse source ini: '.$srcIni);
            return;
        }

        return true;
    }

    /**
     * Checks that Windows built-in extensions are loaded
     *
     * @return bool If a built-in extension is not loaded
     */
    private function checkBuiltIns()
    {
        $exts = array(
            'json',
            'Phar',
            'filter',
            'hash',
            'iconv',
        );

        return $this->getMissingExts($exts);
    }

    /**
     * Returns an array of missing extensions
     *
     * @param array $required
     *
     * @return array
     */
    private function getMissingExts(array $required)
    {
        $result = array();

        foreach ($required as $extension) {
            if (!extension_loaded($extension)) {
                $result[] = $extension;
            }
        }
        return $result;
    }

    /**
     * Returns true if all changes were processed
     *
     * @return bool
     */
    private function processChanges()
    {
        // allow_url_fopen
        if ($set = $this->iniGet('allow_url_fopen', $value)) {
            $set = (bool) $value;
        }

        if (!$set) {
            $this->iniSet('allow_url_fopen', 'On');
        }

        // date.timezone
        if ($set = $this->iniGet('date.timezone', $value)) {
            $set = (bool) $value;
        }

        if (!$set && PHP_MAJOR_VERSION < 7) {
            $this->iniSet('date.timezone', 'UTC');
        }

        // extension_dir
        if (!$extensionDir = $this->getExtensionDir()) {
            return false;
        }

        // extensions
        $exts = array('curl', 'mbstring', 'openssl');

        if ($missing = $this->getMissingExts($exts)) {
            return $this->enableExtensions($missing, $extensionDir);
        }

        return true;
    }

    /**
     * Returns true if all extensions are enabled in the ini file
     *
     * @param array $extensions
     * @param string $extensionDir
     *
     * @return bool
     */
    private function enableExtensions(array $extensions, $extensionDir)
    {
        foreach ($extensions as $name) {
            $dll = $this->extensionGetDllName($name);
            $path = $extensionDir.'\\'.$dll;

            if (!file_exists($path)) {
                $this->writeError('Unable to find extension: '.$path);
                return false;
            }
            $this->iniSet('extension', $name);
        }

        return true;
    }

    /**
     * Returns the extension dll name
     *
     * @param string $name The name of the extension
     *
     * @return string The file name
     */
    private function extensionGetDllName($name)
    {
        return 'php_'.$name.'.dll';
    }

    /**
     * Checks and sets the extension directory if needed
     *
     * @return null|string The extension directory if valid
     */
    private function getExtensionDir()
    {
        $extDir = $this->phpDir.'\\ext';

        if (!file_exists($extDir)) {
            $this->writeError('Normal extension directory does not exist: '.$extDir);
            return;
        }

        if ($this->iniGet('extension_dir', $value)) {
            // Parsing ini content does not use the default value if it is not set

            if (!$path = realpath($value)) {
                // See if we are ext with surrounding forward or back slashes
                if (preg_match('{^(?:\\\\|/)*ext(?:\\\\|/)*$}i', $value)) {
                    $path = $extDir;
                    $this->iniSet('extension_dir', '"ext"');
                } else {
                    $this->writeError('The extension directory does not exist: '.$value);
                    return;
                }
            }

            if (strtolower($path) !== strtolower($extDir)) {
                $this->writeError('Normal extension directory is not being used: '.$path);
                return;
            }

        } else {
            // extension_dir not set in ini
            $this->iniSet('extension_dir', '"ext"');
        }

        return $extDir;
    }

    /**
     * Returns true if name is found and gets the value
     *
     * This operates on the ini data to be modified.
     *
     * @param string $name
     * @param null|string $value Set by method
     *
     * @return bool
     */
    private function iniGet($name, &$value)
    {
        $value = null;

        if ($result = isset($this->iniItems[$name])) {
            $value = $this->iniItems[$name];
        }
        return $result;
    }

    /**
     * Adds an ini value, overwriting any existing value if found
     *
     * @param string $name
     * @param string $value
     */
    private function iniSet($name, $value)
    {
        $match = $this->iniFindExisting($name, $value);

        if ($name === 'extension') {
            $format = '%s=%s';

            // Use the dll name for < PHP7.2
            if (PHP_VERSION_ID < 70200) {
                $value = $this->extensionGetDllName($value);
            }
        } else {
            $format = '%s = %s';
        }

        $line = sprintf($format, $name, $value);

        if (!empty($match)) {
            $start = $match[1];
            $length = strlen($match[0]);
        } else {
            $start = strlen($this->content);
            $length = 0;
            $line = "\n".$line."\n";
        }

        $this->content = substr_replace($this->content, $line, $start, $length);
        $this->changes[] = sprintf('%s=%s', $name, $value);
    }

    /**
     * Returns an array containing the last matched offsets if found
     *
     * @param string $name
     * @param string $value
     *
     * @return array
     */
    private function iniFindExisting($name, $value)
    {
        $result = array();

        if ($name === 'extension') {
            $dll = $this->extensionGetDllName($value);
            $format = '/^\s*;?\s*extension\s*=\s*(?:%s|%s)\s*$/mi';
            $regex = sprintf($format, $value, $dll);
        } else {
            $format = '/^\s*;?\s*%s\s*=.*$/mi';
            $regex = sprintf($format, $name);
        }

        if (!preg_match_all($regex, $this->content, $matches, PREG_OFFSET_CAPTURE)) {
            return $result;
        }

        $matches = $matches[0];
        $active = false;

        foreach ($matches as $match) {
            $line = ltrim($match[0]);

            if ($line[0] !== ';') {
                // Active entry, store it
                $active = true;
                $result = $match;
            } elseif (!$active) {
                // Nothing active, store it so we have the last inactive entry
                $result = $match;
            }
        }
        return $result;
    }

    /**
     * Returns true if an existing ini was backed up
     *
     * @param mixed $new
     *
     * @return bool
     */
    private function iniBackup($new)
    {
        if (!$new) {
            return copy($this->srcIni, $this->origIni);
        }

        return true;
    }

    /**
     * Returns true if the source ini is read
     *
     * @param string $path
     *
     * @return bool
     */
    private function iniRead($path)
    {
        if ($this->content = @file_get_contents($path)) {
            // Normalize line-endings to new-line
            if (false !== strpos($this->content, "\r\n")) {
                $this->content = str_replace("\r\n", "\n", $this->content);
                $this->eol = "\r\n";
            } else {
                $this->eol = "\n";
            }
            // Ensure we have a trailing new-line
            $this->content = rtrim($this->content, "\n")."\n";
        }

        return !empty($this->content);
    }

    /**
     * Returns true if the content is saved to file
     *
     * @return bool
     *
     */
    private function iniSave()
    {
        // Restore original line endings
        if ($this->eol !== "\n") {
            $this->content = str_replace("\n", "\r\n", $this->content);
        }

        return @file_put_contents($this->modIni, $this->content);
    }

    /**
     * Returns true if there are scanned inis and PHP is able to report them
     *
     * php_ini_scanned_files will fail when PHP_CONFIG_FILE_SCAN_DIR is empty.
     * Fixed in 7.1.13 and 7.2.1
     *
     * @return bool
     */
    private function checkScanDirConfig()
    {
        return !(getenv('PHP_INI_SCAN_DIR')
            && !PHP_CONFIG_FILE_SCAN_DIR
            && (PHP_VERSION_ID < 70113
            || PHP_VERSION_ID === 70200));
    }

    /**
     * Writes an error message
     *
     * @param string $message
     */
    private function writeError($message)
    {
        $this->status = sprintf('Error: %s', $message);
    }
}
