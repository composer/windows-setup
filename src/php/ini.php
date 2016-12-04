<?php

$PHP_CHECK_ID = '<ComposerSetup:>';

$ini = new Ini($argv);
$exitCode = $ini->run() ? 0 : 1;

printf('%s%s%s', $PHP_CHECK_ID, $ini->status, PHP_EOL);
exit($exitCode);


class Ini
{
    public $status;
    private $phpDir;
    private $modIni;
    private $origIni;
    private $content;
    private $iniItems;
    private $eol;
    private $changes;

    public function __construct($argv)
    {
        $this->phpDir = $argv[1];
        $tmpDir = $argv[2];

        $this->modIni = $tmpDir.'/php.ini-mod';
        $this->origIni = $tmpDir.'/php.ini-orig';
        $this->changes = array();
        $this->status = '';
    }


    public function run()
    {
        if (!$this->init($new)) {
            return;
        }

        if (!$this->processChanges()) {
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
            $this->status = 'Changes required: '.$changes;
        }

        return true;
    }


    private function init(&$new)
    {
        if ($missing = $this->checkBuiltIns()) {
            $this->writeError('Built-in extensions not avaliable: '.implode(',', $missing));
            return;
        }

        if (!$this->needsChanges()) {
            $this->status = 'No changes to ini file required';
            return;
        }

        // We need to be in the php directory
        if (!chdir($this->phpDir)) {
            $this->writeError('Unable to cd to: '.$this->phpDir);
            return;
        }

        // Make sure we can write to the php directory
        if (!is_writable($this->phpDir)) {
            $this->writeError('Directory is not writable: '.$this->phpDir);
            return;
        }

        if ($srcIni = strval(php_ini_loaded_file())) {
            // We must save a tmp backup
            if (!copy($srcIni, $this->origIni)) {
                $this->writeError('Failed to copy source ini: '.$srcIni);
                return;
            }

        } else {
            $new = true;
            $srcIni = $this->phpDir.'/php.ini-production';
        }

        if (!$this->iniRead($srcIni)) {
            $this->writeError('Failed to read source ini: '.$srcIni);
            return;
        }

        if (!$this->iniItems = parse_ini_string($this->content)) {
            $this->writeError('Failed to parse source ini: '.$srcIni);
            return;
        }

        return true;
    }


    private function needsChanges()
    {
        if (!ini_get('allow_url_fopen')) {
            return true;
        }

        $exts = array('openssl');
        $missing = $this->getMissingExts($exts);

        return !empty($missing);
    }


    /**
     * Checks that Windows built-in extensions are loaded in case of cygwin
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


    private function getMissingExts(array $extensions)
    {
        $result = array();

        foreach ($extensions as $extension) {
            if (!extension_loaded($extension)) {
                $result[] = $extension;
            }
        }
        return $result;
    }


    private function processChanges()
    {
        // allow_url_fopen
        $this->iniGet('allow_url_fopen', $value);

        if (empty($value)) {
            $this->iniSet('allow_url_fopen', 'On');
        }

        // extensions
        $exts = array('openssl', 'mbstring');

        if ($missing = $this->getMissingExts($exts)) {
            return $this->enableExtensions($missing);
        }

        return true;
    }


    /**
     * Returns true if all extensions are enabled in the ini file
     *
     * @param array $extensions
     *
     * @return bool
     */
    private function enableExtensions(array $extensions)
    {
        if (!$this->getExtDir($extDir)) {
            $this->writeError('Unable to find extension dir');
            return false;
        }

        foreach ($extensions as $name) {
            $dll = 'php_'.$name.'.dll';
            $path = $extDir.'/'.$dll;

            if (!file_exists($path)) {
                $this->writeError('Unable to find extension: '.$path);
                return false;
            }
            $this->iniSet('extension', $dll);
        }

        return true;
    }


    /**
     * Returns true if the extension dir is found
     *
     * @param null|string $path Set by method
     *
     * @return bool
     */
    private function getExtDir(&$path)
    {
        if ($this->iniGet('extension_dir', $value)) {
            $path = realpath($value);

            return !empty($path);
        }

        $path = $this->phpDir.'/ext';

        if ($result = file_exists($path)) {
            $this->iniSet('extension_dir', '"ext"');
        }

        return $result;
    }


    /**
     * Returns true if the name is found and sets the value
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

        $format = $name === 'extension' ? '%s=%s' : '%s = %s';
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
            $format = '/^\s*;?\s*%s\s*=\s*%s.*$/mi';
            $regex = sprintf($format, $name, $value);
        } else {
            $format = '/^\s*;?\s*%s\s*=.*$/mi';
            $regex = sprintf($format, $name);
        }

        if (!preg_match_all($regex, $this->content, $matches, PREG_OFFSET_CAPTURE)) {
            return $result;
        }

        $matches = $matches[0];
        $active = false;

        foreach($matches as $match) {
            $line = ltrim($match[0]);

            if ($line[0] !== ';') {
                // Active entry, store it
                $active = true;
                $result = $match;
            } elseif (!$active) {
                // Nothing active, store it so we have last entry
                $result = $match;
            }
        }
        return $result;
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
     * Writes an error message
     *
     * @param string $message
     */
    private function writeError($message)
    {
        $this->status = sprintf('Error: %s', $message);
    }
}
