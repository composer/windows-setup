** Usage Information **

Composer is a PHP script that is run from the command line. To see everything that it can do, open a command window and type: composer

To install a PHP package or library into your project, you first need to create a composer.json file somewhere in your project directory, which tells Composer what to install. Then open a command window and cd (change directory) to its location and type: composer install

This will download the package and any dependencies and put them in a vendor directory. More details can be found at http://getcomposer.org/


** Installation Log **
The Setup program has added the following files to your installation directory:

  -  Added file: composer.bat
  -  Added file: composer.phar
  -  Added file: composer-README.txt
  
Technical details: Your computer does not know how to run the composer.phar script, but it does know how to run a batch file, so a composer.bat file has been created. To make this work when you type "composer", your installation directory has probably been added to your path variable. 


** Uninstalling Composer **
There is no uninstall program so you must manually reverse the changes made above by deleting the files (or the entire directory if appropriate). If the installer added your php directory to the path variable, you should only remove it if you no longer want to run PHP from the command line. If your installation directory was added to the path variable, make sure it is not needed by other programs in this directory (if any) before you remove it.
