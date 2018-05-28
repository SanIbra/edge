# Editor Mode Generator


Editor Mode Generator est un generateur de mode pour editeur de texte.

Mais qu'est ce qu'un mode?
Un mode est un parametrage de votre editeur de texte qui adapte l'editeur a pour un langage donné.
Par exemple un mode pour C permet de mettre les mots clés (commme int if while) en couleur, identer la prochaine ligne si il y a un { etc.


Interet de EDGE.
L'écriture des modes est assez fastidieux et diffère enormement d'un éditeur de texte a un autre. De ce fait, les "petits" langage ne beneficient pas de mode pour tous les editeurs de textes. 
EDGE a pour vocation d'unifier la création de mode. Pour cela il aura besoin d'une description simple du langage (decrite plus tard).Le but est de construire un mode simple comportant les fonctionnalités de base qui pourra être modifiable par l'utilisateur.


Pour l'instant EDGE peut créer des modes pour emacs, vim et vscode.

# Requirement

EDGE est ecrit en haskell. Il faut donc avoir Haskell installer sur votre machine.

Au minimun la version 8.2.2

  

Vous pouvez telecharger haskell sur le site officiel sur le lien ci-dessous.

  

<https://docs.haskellstack.org/en/stable/install_and_upgrade/>



Le mode Emacs necessite la version 25.3 ou plus pour fonctionne telechargeable sur le lien ci-dessous.

<https://www.gnu.org/software/emacs/download.html>  

Le mode Vim necessite la version 7.3 ou plus pour fonctionne telechargeable sur le lien ci-dessous.

<https://www.vim.org/download.php>
  
Le mode VsCode necessite la version 1.22.2 ou plus pour fonctionne telechargeable sur le lien ci-dessous.
<https://code.visualstudio.com/download>

## Fonctionnement
Il faut dans un premier temps créer le fichier .edge qui decrit la syntaxe de votre langage.
commme dans l'exemple ci-dessous.

```
name : #name ;
extension : #extension ;
keywords : #word , #word2 ;
constants : #constant1 , #constant2 ;
operators : #op1 , #op2 , #op3 ;
idents : #REGEX ;
numbers : #REGEX2 ;
peers : #ouvrante #fermante ;
commentLine : #Symbole ;
commentBlock : #REGEXDebut #REGEXfin
```
Par exemple pour le langage C le descripteur serait:

```
name : #C ;
extension : #c;
keywords : #if , #else , #while , #do , #int , #float;
constants : #true , #false ;
operators : #+ , #- , #/ ;
idents : #[a-z]* ;
numbers : #[0-9]+\.[0-9]* ;
peers : #{ #} , #( #) ;
commentLine : #// ;
commentBlock : #/* #*/
```


ATTENTION: les mots clés sont tous separes par des espaces et tous les champs doivent etre renseignes
### Generation du mode

lancer le ghci sur votre console
```
ghci
```
  
charge le edge
```
:l EDGE.hs
```

generer le mode:

```
edgeGen "descripeur" "mode"
```

Remplacer descripeur par le chemin du fichier descripteur et le mode par le mode que vous voulez generer.

Vous pouvez afficher les modes generable avec la commande :

```
listMode
```
Pour l'instant seul les modes de emacs vim et vscode peuvent etre generer.
 
Voila votre mode est generer il suffit maintenant l'installer sur votre editeur preferer.

### Installation de mode

##Emacs

Pour tester le mode: il suffit d'ouvrir le fichier .el generer par edge
rentre la commmande dans le console d'emacs
```
eval-buffer
```

ensuite ouvrir un fichier pour tester le mode.
Charger le mode avec la commande 
```
<nom du mode>-mode.el
```
"nom du mode" est l'attribut name dans le decripteur

Pour que le mode se charger automatiquement il faut mettre dans votre fichier init.
Le fichier init a different nom et emplacement je vous laisse ce guide pour le trouver
<https://www.gnu.org/software/emacs/manual/html_node/efaq-w32/Location-of-init-file.html#Location-of-init-file>


Ensuite rajouter les 2 lignes qui sont en commentaire au debut du mode

##Vim
Sous windows :
Si vous utilisez un package manager, copiez le dossier genere dans le dossier contenant les plugins ("~/vimfiles/bundle" pour pathogen).

Sinon, copiez chaque fichier du plugin dans le dossier correspondant (du dossier indent vers "~/vimfiles/indent", ftdetect vers "~/vimfiles/ftdetect" etc...).

Sous Linux :
Si vous utilisez un package manager, copiez le dossier genere dans le dossier contenant les plugins ("~/.vim/bundle" pour pathogen).

Sinon, copiez chaque fichier du plugin dans le dossier correspondant (du dossier indent vers "~/.vim/indent", ftdetect vers "~/.vim/ftdetect" etc...).

##VsCode
Copier le dossier genere dans le repertoire "~/.vscode/extension".
