library(ggplot2) #do wykres�w
library(dplyr) #do summarize, group_by



#--------------------------------------------------------------------------------------------------
# czytanie i obrobka danych --  Bolk�w
#--------------------------------------------------------------------------------------------------

# czytanie danych surowych z pliku -- BOLK �w
#------------------------------------------------------------------------------------------
  DaneMeteo <- read.csv("Bolkow_DOPADP.dat",header = FALSE, sep=",")
    head(DaneMeteo)
    str(DaneMeteo)


  colnames(DaneMeteo) <- c('Kod','Rok','Mies','Dzien','S','Opady_sum','Pusta')
    #head(DaneMeteo)

  Ilosci_dni_w_mies <-  DaneMeteo %>%
                        ##filter(Rok == 1981) %>%
                        group_by(Rok,Mies) %>%
                        summarise(
                          count = n()

                        ) %>%
                        arrange(Rok,Mies)



# Dodaj dodatkowe kolumny do surowych danych
#------------------------------------------------------------------------------------------
  DaneMeteo <- cbind(
                      DaneMeteo$Rok,
                      DaneMeteo$Mies,
                      DaneMeteo$Dzien,
                      DaneMeteo$Opady_sum,
                      matrix(0,nrow(DaneMeteo),4)
                      )

  colnames(DaneMeteo) <- c(
                          'Rok',
                          'Mies',
                          'Dzien',
                          'Opady_sum',
                          'Poprzedni_Czy_Opad',
                          'Czy_Opad',
                          'D_W',
                          'W_W'
                          )
    #head(DaneMeteo)
    #str(DaneMeteo)
  DaneMeteo <- data.frame(DaneMeteo)
    #head(DaneMeteo)
    #str(DaneMeteo)

# Czy_Opad -- warto�ci 0,1 - czy w biezacym dniu by�� opad
#------------------------------------------------------------------------------------------
  DaneMeteo$Czy_Opad[which(DaneMeteo$Opady_sum  > 0)] <- 1
  DaneMeteo$Czy_Opad[which(DaneMeteo$Opady_sum == 0)] <- 0
  DaneMeteo$Czy_Opad[which(DaneMeteo$Opady_sum  < 0)] <- NA


# Poprzedni_Czy_Opad -- warto�ci 0,1 - czy w poprzednim dniu by�� opad
#------------------------------------------------------------------------------------------

  # dla pierwszego dnia danych nie mamy informacji o poprzednim
  DaneMeteo$Poprzedni_Czy_Opad[1] <- NA

  # kopiuję dane z poprzedniego dnia
  for (i in 2:nrow(DaneMeteo))
  {
    if(DaneMeteo$Dzien[i] - DaneMeteo$Dzien[i-1] == 1){DaneMeteo$Poprzedni_Czy_Opad[i] <- DaneMeteo$Czy_Opad[i-1]}
    else{DaneMeteo$Poprzedni_Czy_Opad[i] <- NA}
  }


# D_W - wartości 0 , 1 (pierwsza NA) - Czy było przejscie z Dry to Wet
#------------------------------------------------------------------------------------------

  # jesli nie mamy informacji czy był opad w poprzednim dniu wstawiamy NA (np dla pierwszego dnia danych nie mamy informacji o poprzednim
  DaneMeteo$D_W[which(is.na(DaneMeteo$Poprzedni_Czy_Opad))] <- NA
  DaneMeteo$D_W[which(DaneMeteo$Poprzedni_Czy_Opad  == 0 & DaneMeteo$Czy_Opad  == 1)] <- 1

# W_W - wartości 0 , 1 (pierwsza NA) - Czy było przejscie z Dry to Wet
#------------------------------------------------------------------------------------------

  # jesli nie mamy informacji czy był opad w poprzednim dniu wstawiamy NA (np dla pierwszego dnia danych nie mamy informacji o poprzednim
  DaneMeteo$W_W[which(is.na(DaneMeteo$Poprzedni_Czy_Opad))] <- NA
  DaneMeteo$W_W[which(DaneMeteo$Poprzedni_Czy_Opad  == 1 & DaneMeteo$Czy_Opad  == 1)] <- 1


      #head(DaneMeteo)

      #DaneMeteo[which(DaneMeteo$Rok == 1981),]



#--------------------------------------------------------------------------------------------------
# estymacja parametrów modelu
#--------------------------------------------------------------------------------------------------


                    #sprawdzam jak wyglada średnia dla wybranych miesięcy na przełomie lat
                    #------------------------------------------------------------------------------------------

                    Opady_mean_w_latach <-  DaneMeteo %>%
                                          #filter(Mies == 1) %>%
                                          group_by(Mies,Rok) %>%
                                          summarise(Mean = mean(Opady_sum)) %>%
                                          arrange(Mies,Rok)

                    #head(Opady_mean_w_latach)

                    # styczen
                    ggplot(Opady_mean_w_latach[which(Opady_mean_w_latach$Mies == 1),], aes(x=Rok, y=Mean)) +
                      geom_bar(stat="identity")

                    # luty
                    ggplot(Opady_mean_w_latach[which(Opady_mean_w_latach$Mies == 2),], aes(x=Rok, y=Mean)) +
                      geom_bar(stat="identity")

                    # marzec
                    ggplot(Opady_mean_w_latach[which(Opady_mean_w_latach$Mies == 3),], aes(x=Rok, y=Mean)) +
                      geom_bar(stat="identity")

                    # kwiecien
                    ggplot(Opady_mean_w_latach[which(Opady_mean_w_latach$Mies == 4),], aes(x=Rok, y=Mean)) +
                      geom_bar(stat="identity")

                    # 5
                    ggplot(Opady_mean_w_latach[which(Opady_mean_w_latach$Mies == 5),], aes(x=Rok, y=Mean)) +
                      geom_bar(stat="identity")

                    # 6
                    ggplot(Opady_mean_w_latach[which(Opady_mean_w_latach$Mies == 6),], aes(x=Rok, y=Mean)) +
                      geom_bar(stat="identity")

                    # 7
                    ggplot(Opady_mean_w_latach[which(Opady_mean_w_latach$Mies == 7),], aes(x=Rok, y=Mean)) +
                      geom_bar(stat="identity")

                    # 8
                    ggplot(Opady_mean_w_latach[which(Opady_mean_w_latach$Mies == 8),], aes(x=Rok, y=Mean)) +
                      geom_bar(stat="identity")

                    # 9
                    ggplot(Opady_mean_w_latach[which(Opady_mean_w_latach$Mies == 9),], aes(x=Rok, y=Mean)) +
                      geom_bar(stat="identity")

                    # 10
                    ggplot(Opady_mean_w_latach[which(Opady_mean_w_latach$Mies == 10),], aes(x=Rok, y=Mean)) +
                      geom_bar(stat="identity")

                    # 11
                    ggplot(Opady_mean_w_latach[which(Opady_mean_w_latach$Mies == 11),], aes(x=Rok, y=Mean)) +
                      geom_bar(stat="identity")

                    # 12
                    ggplot(Opady_mean_w_latach[which(Opady_mean_w_latach$Mies == 12),], aes(x=Rok, y=Mean)) +
                      geom_bar(stat="identity")



                  #Średnia ilości opadów w podziale na miesiące
                  #------------------------------------------------------------------------------------------

                  Opady_mean <-  DaneMeteo %>%
                                  group_by(Mies) %>%
                                  summarise(Mean = mean(Opady_sum)) %>%
                                  arrange(Mies)

                  ggplot(Opady_mean[], aes(x=Mies, y=Mean)) +
                    geom_bar(stat="identity")


  #Estymacja prawdopodobieństw przejścia - z próby
  #------------------------------------------------------------------------------------------

  Pr_przejscia <-  DaneMeteo[-1,] %>% #bez pierwszego dnia bo nie jesteśmy w stanie określić poprzedniego
                      filter(is.na(Poprzedni_Czy_Opad) == FALSE) %>% # bez braków danych dla poprzedniego dnia
                      group_by(Mies) %>%
                      summarise(
                                count = n(),## ilosc obserwowanych dni - z danymi
                                D_W_sum = sum(D_W), ##ilosc dni gdzie było przejcie z Dry na Wet
                                W_W_sum = sum(W_W), ##ilopsc dni gdzie było przejście z Wet na Wet
                                D_pop_sum = n() - sum(Poprzedni_Czy_Opad), ##ilosc dni gdzie poprzedni był Dry
                                W_pop_sum = sum(Poprzedni_Czy_Opad), ##ilosc dni gdzie poprzedni był Wet
                                D_W_pr = sum(D_W)/(n()-sum(Poprzedni_Czy_Opad)), ## frakcja z Dry na Wet
                                W_W_pr = sum(W_W)/sum(Poprzedni_Czy_Opad)## frakcja z Wet na Wet

                                ) %>%
                      arrange(Mies)

  ggplot(Pr_przejscia, aes(x=D_W_pr, y=W_W_pr)) +
    geom_line(aes(group=1)) +     # Group all points; otherwise no line will show
    geom_point(size=3)



  #Estymacja prawdopodobieństwa dnia mokrego P(W) - z próby
  #------------------------------------------------------------------------------------------

  Pr_opadu <-  DaneMeteo[] %>%
                  group_by(Mies) %>%
                  summarise(
                            count = n(),
                            W_sum = sum(Czy_Opad),
                            W_pr = sum(Czy_Opad)/n()
                            ) %>%
                  arrange(Mies)

  ggplot(Pr_opadu[], aes(x=Mies, y=W_sum)) +
    geom_bar(stat="identity")

  #Zebranie danych z proby
  #------------------------------------------------------------------------------------------
  Prawdopodobienstwa_z_proby <- cbind(
                                      Pr_przejscia$Mies,
                                      Pr_przejscia$D_W_pr,
                                      Pr_przejscia$W_W_pr,
                                      Pr_opadu$W_pr ## fraction of a wet day
                                      )

  colnames(Prawdopodobienstwa_z_proby) <- c(
                                          'Mies',
                                          'D_W_pr',
                                          'W_W_pr',
                                          'W_pr'
                                          )
  Prawdopodobienstwa_z_proby <- data.frame(Prawdopodobienstwa_z_proby)


  ### --- uwaga! na pierwszy rzut oka podejrzewam,
      #że lepiej zrobić regresje do zalezności P(W|W) = a + bP(W) niż P(W|D) = a + bP(W)
  #------------------------------------------------------------------------------------------

  ## P(W|D) = a + bP(W)
  ggplot(Prawdopodobienstwa_z_proby, aes(x=W_pr, y=D_W_pr)) +
    geom_point(size=3)

  model1 <- lm(formula = Prawdopodobienstwa_z_proby$D_W_pr ~ Prawdopodobienstwa_z_proby$W_pr)

      model1
      summary(model1)


  #------------------------------------------------------------------------------------------
  ## P(W|W) = a + bP(W)
  ggplot(Prawdopodobienstwa_z_proby, aes(x=W_pr, y=W_W_pr)) +
    geom_point(size=3)

  model2 <- lm(formula = Prawdopodobienstwa_z_proby$W_W_pr ~ Prawdopodobienstwa_z_proby$W_pr)

      model2
      summary(model2)


  ## teraz czas na obliczenia na papierze :) (używam drugiego modelu, na pierwszy za mało danych więc słaby wyszedł)
  #------------------------------------------------------------------------------------------

    #  P(W|W) = 0.3969 + 0.6039 * P(W)
    #  P(W) = P(W|D) * Py(D) + P(W|W) * Py(W)  - zakładamy że Py(W) = P(W) zatem również Py(D) = 1 - P(W)
    #  P(W) = P(W|D) * (1 - P(W)) + P(W|W) * P(W)
    #  P(W|D) = [ P(W) - P(W|W) * P(W) ] / ( 1 - P(W) )
    #  P(W|D) = [ P(W) - (0.3969 + 0.6039 * P(W)) * P(W) ] / ( 1 - P(W) )
    #  P(W|D) = [ P(W) - 0.3969 * P(W) - 0.6039 * P(W) * P(W) ] / ( 1 - P(W) )
    #  P(W|D) = P(W) * [0.6031 - 0.6039 * P(W)] / ( 1 - P(W) )
    # przyjmując że  0.6031 =  0.6039 = 0.6 :) dostajemy (w publikacji też zaokrąglali ;)

    #  P(W|D) = 0.6 * P(W) Hura :):)


  ## obliczamy wyestymowane parametry dla kazdego miesiaca
  #------------------------------------------------------------------------------------------

      parametr_P_W_W <- 0.3969 + 0.6039 * Prawdopodobienstwa_z_proby$W_pr
      # dla porównania z proby: Prawdopodobienstwa_z_proby$W_W_pr

      parametr_P_D_W <- 0.6 * Prawdopodobienstwa_z_proby$W_pr
      # dla porównania z proby: Prawdopodobienstwa_z_proby$D_W_pr
      # uwaga: mocno odstaje - ciekawe czy to dlatego ze gdzieś bląd, czy dlatego, że na tyle mała proba, ze slabe wychodza prawd z proby??


#--------------------------------------------------------------------------------------------------
# symulacja przyszłych dni (Dry or Wet) na podstawie przejśc w łańcuchu Markowa
#--------------------------------------------------------------------------------------------------



  #kalendarz
  #------------------------------------------------------------------------------------------

      daty <- seq(as.Date("2017-01-01"), as.Date("2057-12-31"), "days")
      dni <- as.integer(format(daty,"%d"))
      miesiace <- as.integer(format(daty,"%m"))
      lata <- as.integer(format(daty,"%Y"))

      Kalendarz <- cbind(
                          lata,
                         miesiace,
                         dni
                         )

      colnames(Kalendarz) <- c(
                                'Rok',
                                'Mies',
                                'Dzien'
                                )

      head(Kalendarz)


  #symulacja
  #------------------------------------------------------------------------------------------

  # losowy wektor prawdopodobieństw
      losowanie <- runif(nrow(Kalendarz), 0.0, 1.0)

  # tabel na podstawie której i do której będę wrzucać symulacje
    symulacja <- cbind(
                               Kalendarz,
                               losowanie,
                               matrix(0,nrow(Kalendarz),3) #Poprzedni_Czy_Opad, Czy_Opad, Opady_sum
                              )

    colnames(symulacja) <- c(
                                        'Rok',
                                        'Mies',
                                        'Dzien',
                                        'Losowanie',
                                        'Poprzedni_Czy_Opad',
                                        'Czy_Opad',
                                        'Opady_sum'
                                      )

    symulacja <- data.frame(symulacja)
    head(symulacja)
    str(symulacja)

    # pętla symulacji:
    #------------------------------------------------------------

    #parametry do symulacji:
      #parametr_P_W_W
      #parametr_P_D_W


    symulacja$Poprzedni_Czy_Opad[1] <- 1 # ustalam że dla pierwszego dnia symulacji, poprzedni miał opad

    for (i in 1:nrow(symulacja))
    {
           if(symulacja$Poprzedni_Czy_Opad[i] == 1 & symulacja$Losowanie[i] < parametr_P_W_W[symulacja$Mies[i]]){symulacja$Czy_Opad[i] <- 1 }
      else if(symulacja$Poprzedni_Czy_Opad[i] == 1 & symulacja$Losowanie[i] >= parametr_P_W_W[symulacja$Mies[i]]){symulacja$Czy_Opad[i] <- 0 }

      else if(symulacja$Poprzedni_Czy_Opad[i] == 0 & symulacja$Losowanie[i] < parametr_P_D_W[symulacja$Mies[i]]){symulacja$Czy_Opad[i] <- 1 }
      else if(symulacja$Poprzedni_Czy_Opad[i] == 0 & symulacja$Losowanie[i] >= parametr_P_D_W[symulacja$Mies[i]]){symulacja$Czy_Opad[i] <- 0 }
      else{print ('error')}

      if (i >= 2 & i <= (nrow(symulacja) - 1)) {symulacja$Poprzedni_Czy_Opad[i+1] <- symulacja$Czy_Opad[i]}

      #print(parametr_P_W_W[symulacja$Mies[i]])
      #print(parametr_P_D_W[symulacja$Mies[i]])
      #print(symulacja$Mies[i])
      #print(symulacja$Losowanie[i])
      #print(symulacja$Poprzedni_Czy_Opad[i])
      }

    head(symulacja)

                    #prawdopodobieństwa dnia mokrego P(W) - w symulacji
                    #------------------------------------------------------------------------------------------

                    Pr_opadu_symulacja <-  symulacja[] %>%
                                          group_by(Mies) %>%
                                          summarise(
                                            count = n(),
                                            W_sum = sum(Czy_Opad),
                                            W_pr = sum(Czy_Opad)/n()
                                          ) %>%
                      arrange(Mies)

                    Pr_opadu_symulacja # symulacja
                    Pr_opadu # próba prawdziwa

                    ggplot(Pr_opadu_symulacja[], aes(x=Mies, y=W_pr)) +
                      geom_bar(stat="identity")

                    ggplot(Pr_opadu[], aes(x=Mies, y=W_pr)) +
                      geom_bar(stat="identity")

#--------------------------------------------------------------------------------------------------
# Symulacja wartości opadów w dni mokre - rozklad Gamma
#--------------------------------------------------------------------------------------------------


    #estymacja beta  i alfa  z proby
    #--------------------------------------------------------------------------------------------------


    #WZOR
    #-----------------------------------------------
    # a = (0.5000876 + 0.16488552 * Y - 0.0544274 * Y^2) / Y
    # b = X/a

    # gdzie
    #  Y = ln(X/G)
    #  X - Srednia arytmetyczna opadów w dni mokre
    #  G - średnia geometryczna opadów w dni mokre


    # wyznaczam X i Y z próby, dla każdego miesiąca
    #-----------------------------------------------
           #   "geometric.mean" <-
            #    function(x,na.rm=TRUE){ if (is.null(nrow(x))) {exp(mean(log(x),na.rm=TRUE)) } else {
             #     exp(apply(log(x),2,mean,na.rm=na.rm))} }

    Opady_mean_per_Wet <-  DaneMeteo %>%
                            filter(Czy_Opad == 1) %>%
                            group_by(Mies) %>%
                            summarise(
                                        X  = mean(Opady_sum),

                                        G = exp(mean(log(Opady_sum))),

                                        # taka sztuczka, dodaje tu od razu potrzebne wektory potem, zeby się trzymały razem
                                        Y = 0,
                                        alfa = 0,
                                        beta = 0,

                                        alfa_est = 0,
                                        beta_est = 0
                                        ##G = geometric.mean(Opady_sum)
                                      ) %>%
                            arrange(Mies)



    # wyznaczam alfa i beta
    #-----------------------------------------------

    Opady_mean_per_Wet$Y <- log(Opady_mean_per_Wet$X/Opady_mean_per_Wet$G)

    Opady_mean_per_Wet$alfa <- (0.5000876 + 0.16488552 * Opady_mean_per_Wet$Y - 0.0544274 * Opady_mean_per_Wet$Y^2) / Opady_mean_per_Wet$Y

    Opady_mean_per_Wet$beta <-  Opady_mean_per_Wet$X / Opady_mean_per_Wet$alfa


                      # sprawdzam na wykresie czy rzeczywiście róWnoległe
                      #-----------------------------------------------
                      ggplot(Opady_mean_per_Wet[], aes(x=Mies, y=X)) +
                        geom_line(stat="identity")

                      ggplot(Opady_mean_per_Wet[], aes(x=Mies, y=beta)) +
                        geom_line(stat="identity")

                      plot(Opady_mean_per_Wet$Mies, Opady_mean_per_Wet$X, type="b",col="red") #,main="Wykres"
                      # a następnie dorysowujemy do niej druga
                      lines(Opady_mean_per_Wet$Mies, Opady_mean_per_Wet$beta, col="blue", type="l")


                      ggplot(Opady_mean_per_Wet, aes(x=X, y=beta)) +
                        geom_point(size=3)


    # OK, założenie i liniowości przyjmuję jako zasadne, używam rekgresji do estymacji alfa i beta
    #-----------------------------------------------

    model_3 <- lm(formula = Opady_mean_per_Wet$beta ~ Opady_mean_per_Wet$X)

    model_3
    summary(model_3)

    Opady_mean_per_Wet$beta_est  <- coefficients(model_3)[1] + coefficients(model_3)[2] * Opady_mean_per_Wet$X
    Opady_mean_per_Wet$alfa_est  <- Opady_mean_per_Wet$X / Opady_mean_per_Wet$beta_est



  #ostateczna estymacja ilości opadów w dni mokre
  #--------------------------------------------------------------------------------------------------

                          head(symulacja)
                          #symulacja$Opady_sum <- 0

                          ##rgamma(100, Opady_mean_per_Wet$alfa_est[1], scale = Opady_mean_per_Wet$beta_est[1])

    for (i in 1:nrow(symulacja))
    {
      if(symulacja$Czy_Opad[i] == 1){symulacja$Opady_sum[i] <- rgamma(1, Opady_mean_per_Wet$alfa_est[symulacja$Mies[i]], scale = Opady_mean_per_Wet$beta_est[symulacja$Mies[i]])}

      #print(symulacja$Czy_Opad[i] == 1)
      #print(rgamma(1, Opady_mean_per_Wet$alfa_est[1], scale = Opady_mean_per_Wet$beta_est[1]))
      #print(rgamma(1, Opady_mean_per_Wet$alfa_est[symulacja$Mies[i]], scale = Opady_mean_per_Wet$beta_est[symulacja$Mies[i]]))
    }

    symulacja[1:100,]

                            # sprawdzam
                            Opady_mean_per_Wet_symulacja <-  symulacja %>%
                                                                filter(Czy_Opad == 1) %>%
                                                                group_by(Mies) %>%
                                                                summarise( X  = mean(Opady_sum)) %>%
                                                                arrange(Mies)

                            Opady_mean_per_Wet_symulacja
                            Opady_mean_per_Wet