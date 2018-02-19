      program mailleur_optimal
      
! Atmo Rhone-Alpes 2009
! Description : le program lit un fichier .MIF et cr�e un maillage intelligent
!               pour un mod�le de rue type SIRANE
!              le maillage intelligent est d�fini par une distance � l'axe et
!              une distance inter-point (ex: � 50 m de l'axe, un point tous les 
!              20 m, � 1000 m de l'axe, un point tous les 1000 m)
! Principe : 1. lecture du r�seau dans le fichier .MIF
!            2. calcul des param�tres des brins : �tendue du domaine, longueurs
!            2. discr�tisation des brins en points (parametres : dl_brin = 10 m)
!            3. d�finition d'un maillage initial
!            4. filtrage des points utilis�s

      use params

      implicit none
      
      ! ON A POSE LES BASES C'EST PARTI !!!!

      ! lit le fichier
      call read_args
      
      ! lit le fichier
      call read_mif   
      
      ! calcul les parametres des brins  
      call calcul_brins
      
      ! calcul les parametres des recepteurs  
      call calcul_recept
      
      end program
      
          
