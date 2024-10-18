CREATE OR REPLACE FUNCTION inserimento_articolo()
    RETURNS trigger
    LANGUAGE plpgsql
    AS $function$
        BEGIN
			NEW.data_creazione := NOW();
            RETURN NEW;
        END;
    $function$
    ;


CREATE TRIGGER creazione_articolo BEFORE
INSERT ON articoli FOR EACH ROW EXECUTE FUNCTION inserimento_articolo();




CREATE OR REPLACE FUNCTION inserimento_modifica()
    RETURNS trigger
    LANGUAGE plpgsql
    AS $function$
        DECLARE autore_articolo_modifica articoli.autore_articolo%TYPE;
        articolo_originale articoli.titolo%TYPE := (get_articolo_from_frase(NEW.frase_modifica));
        posizione_partenza_spostamento INTEGER;
        posizione_massima INTEGER := (SELECT COUNT(*) FROM modifiche AS m
                                      INNER JOIN frasi AS f
                                      ON m.frase_modifica = f.id_frase
                                      WHERE f.articolo_contenitore = articolo_originale AND 
                                      m.accettazione = true AND
                                      m.frase_modifica != NEW.frase_modifica AND
                                      data_aggiornamento = get_max_data_aggiornamento(m.frase_modifica));
        BEGIN
            IF NEW.posizione < -1
                THEN RAISE EXCEPTION 'La posizione di una frase non può essere minore di -1';
            END IF;

            CALL controlli_modifiche_base(NEW);
            IF NEW.posizione > posizione_massima
            THEN
                RAISE EXCEPTION 'Posizione modifica > posizione massima possibile per questo articolo';
            END IF;


            NEW.accettazione := false;
            autore_articolo_modifica  := (SELECT autore_articolo FROM articoli
                                    INNER JOIN frasi
                                    ON articoli.titolo = frasi.articolo_contenitore
                                    WHERE frasi.id_frase = NEW.frase_modifica);
            IF NEW.autore_modifica = autore_articolo_modifica
            THEN
                IF NOT get_existence_modifiche_in_attesa_in_articolo (NOW()::timestamp, articolo_originale) THEN
                    
                    IF NEW.posizione = -1
                    THEN
                        posizione_partenza_spostamento :=
                            (SELECT posizione FROM modifiche
                            WHERE frase_modifica = NEW.frase_modifica AND
                            accettazione = true AND
                            data_aggiornamento = get_max_data_aggiornamento(NEW.frase_modifica));
                    ELSE
                        posizione_partenza_spostamento := NEW.posizione;
                    END IF;
                    
                    NEW.data_accettazione_frase := NOW();
                    NEW.data_aggiornamento := NOW();
                    NEW.accettazione := true;

                    CALL spostamento_modifiche (CASE WHEN NEW.posizione = -1 THEN -1 ELSE 1 END,
                        articolo_originale,
                        NEW.frase_modifica,
                        posizione_partenza_spostamento);

                    END IF;
                ELSE
                    IF NEW.accettazione = false
                    THEN
                        NEW.data_accettazione_frase := NULL;
                        NEW.data_aggiornamento := NULL;
                        -- INSERT INTO ordinamento_modifiche (modifica_da_ordinare)
                        -- VALUES (NEW.id_modifica);
                    END IF;
                END IF;
                NEW.data_creazione := NOW();
                RETURN NEW;
        END;
    $function$
    ;



CREATE TRIGGER creazione_modifica BEFORE
INSERT ON modifiche FOR EACH ROW EXECUTE FUNCTION inserimento_modifica();



CREATE OR REPLACE FUNCTION aggiornamento_modifica() RETURNS trigger
LANGUAGE plpgsql
AS $function$
    DECLARE
        steps INTEGER := 1;
        old_posizione INTEGER;
        articolo_riferimento frasi.articolo_contenitore%TYPE;
        riga_ordinamento ordinamento_modifiche%ROWTYPE;
        old_data_accettazione modifiche.data_accettazione_frase%TYPE;
        data_aggiornamento_mod_originale modifiche.data_aggiornamento%TYPE:= NOW();
        scaling_mod INTEGER;
        formuletta INTEGER;
    BEGIN
        IF is_modifica_revisionata(OLD, NEW)
        THEN
            RAISE EXCEPTION 'Modifica già revisionata/non modificabile';
        END IF;

        articolo_riferimento := get_articolo_from_frase(OLD.frase_modifica);

        IF get_existence_modifiche_in_attesa_in_articolo (OLD.data_creazione, articolo_riferimento) THEN
            RAISE EXCEPTION 'Altre modifiche da revisionare per questo testo';
        END IF;


        SELECT * INTO riga_ordinamento
        FROM ordinamento_modifiche
        WHERE modifica_da_ordinare = OLD.id_modifica;

        IF NEW.accettazione = TRUE
        THEN
            IF riga_ordinamento IS NULL
            THEN
                RAISE EXCEPTION 'Frase non ordinabile (manca una entry in ordinamento_modifiche)';
            END IF;
            IF riga_ordinamento.visionato = FALSE
            THEN
                RAISE EXCEPTION 'Frase non ordinata';
            END IF;
            
            CALL controlli_modifiche_base(NEW);

            

            old_data_accettazione := (SELECT data_accettazione_frase
                                      FROM modifiche
                                      WHERE frase_modifica = NEW.frase_modifica AND
                                      accettazione = true AND
                                      data_aggiornamento = get_max_data_aggiornamento(NEW.frase_modifica));
            

            old_posizione := CASE WHEN NEW.posizione = -1 THEN 
                            get_ultima_posizione_positiva(NEW.frase_modifica, NOW()::timestamp - INTERVAL '1 microsecond')
                            ELSE NEW.posizione
                            END;

            scaling_mod := scaling_modifiche(old_posizione, NEW.data_creazione, articolo_riferimento);

            data_aggiornamento_mod_originale :=
                CASE WHEN scaling_mod != 0
                THEN
                (NOW() - INTERVAL '1 millisecond')
                ELSE
                NOW()
                END;    
            IF old_data_accettazione IS NULL
                THEN
                    NEW.data_accettazione_frase := data_aggiornamento_mod_originale;
                ELSE
                    NEW.data_accettazione_frase := old_data_accettazione;
                END IF;
            NEW.data_aggiornamento := data_aggiornamento_mod_originale;

            formuletta := (scaling_mod -
                          (get_modifiche_in_stessa_posizione(old_posizione, NEW.data_creazione, articolo_riferimento) - riga_ordinamento.offset_posizione));

            RAISE NOTICE 'old_posizione = %', old_posizione;

        
            RAISE NOTICE 'formuletta: % - (% - %) = %', scaling_mod,
                get_modifiche_in_stessa_posizione(old_posizione, NEW.data_creazione, articolo_riferimento), riga_ordinamento.offset_posizione,
                formuletta;

            IF (NEW.posizione = -1) THEN
                steps := -1;
            ELSE
                IF riga_ordinamento.frase_raccordo_sinistra IS NOT NULL THEN
                    CALL insert_raccordo (FALSE, NEW.posizione, riga_ordinamento);
                END IF;

                IF riga_ordinamento.frase_raccordo_destra IS NOT NULL THEN
                    CALL insert_raccordo (TRUE, NEW.posizione, riga_ordinamento);
                END IF;

                DROP TRIGGER IF EXISTS creazione_modifica ON modifiche;

                IF scaling_mod != 0
                THEN
                    INSERT INTO modifiche (
                        data_creazione,
                        posizione,
                        accettazione,
                        data_accettazione_frase,
                        data_aggiornamento,
                        autore_modifica,
                        frase_modifica,
                        collegamento
                    ) VALUES (
                        NEW.data_creazione,
                        NEW.posizione + formuletta,
                        NEW.accettazione,
                        NEW.data_accettazione_frase,
                        NOW(),
                        NEW.autore_modifica,
                        NEW.frase_modifica,
                        NEW.collegamento
                    );
                END IF;

                CREATE TRIGGER creazione_modifica BEFORE
                INSERT ON modifiche FOR EACH ROW EXECUTE FUNCTION inserimento_modifica();
            END IF;
            
        
        DELETE FROM ordinamento_modifiche WHERE modifica_da_ordinare = NEW.id_modifica;
        END IF;


        CALL spostamento_modifiche(steps + riga_ordinamento.offset_posizione,
            articolo_riferimento,
            NEW.frase_modifica,
            old_posizione + formuletta);

        RETURN NEW;
    END;
$function$;

CREATE TRIGGER revisione_modifiche BEFORE
UPDATE ON modifiche FOR EACH ROW EXECUTE FUNCTION aggiornamento_modifica();




CREATE OR REPLACE FUNCTION inserimento_ordinamento_modifiche ()
    RETURNS trigger
    LANGUAGE plpgsql
    AS $function$
	BEGIN
        NEW.offset_posizione := 0;
        NEW.frase_raccordo_sinistra := NULL;
        NEW.frase_raccordo_destra := NULL;
        NEW.visionato := false;
        RETURN NEW;
	END;
    $function$;

CREATE TRIGGER creazione_ordinamento_modifiche BEFORE
INSERT ON ordinamento_modifiche FOR EACH ROW EXECUTE FUNCTION inserimento_ordinamento_modifiche();




CREATE OR REPLACE FUNCTION aggiornamento_ordinamento_modifiche ()
    RETURNS trigger
    LANGUAGE plpgsql
    AS $function$
    DECLARE
		data_inserimento_modifica modifiche.data_creazione%TYPE :=
        (SELECT data_creazione FROM modifiche WHERE id_modifica = NEW.modifica_da_ordinare);
    	articolo articoli.titolo%TYPE := (SELECT articolo_contenitore FROM frasi
										  INNER JOIN modifiche
										  ON id_frase = frase_modifica
										  WHERE id_modifica = NEW.modifica_da_ordinare);
		posizione_originale INTEGER := (SELECT posizione
										FROM modifiche
										WHERE id_modifica = NEW.modifica_da_ordinare);
	BEGIN
        IF NEW.visionato = FALSE
        THEN RAISE EXCEPTION 'visionato non può essere false';
        END IF;

        IF NEW.offset_posizione < 0 OR
            NEW.offset_posizione > (SELECT COUNT(*)
                                    FROM modifiche
									INNER JOIN frasi
									ON frase_modifica = id_frase
                                    WHERE data_aggiornamento >= data_inserimento_modifica AND
									accettazione = true AND
									articolo_contenitore = articolo AND
									posizione = posizione_originale
                                    )
        THEN
            RAISE EXCEPTION 'non si può avere offset_posizione minore di 0 o maggiore delle modifiche accettate destinate a quella posizione';
        END IF;

        RETURN NEW;
    END;
    $function$;

CREATE TRIGGER revisione_ordinamento_modifiche BEFORE
UPDATE ON ordinamento_modifiche FOR EACH ROW EXECUTE FUNCTION aggiornamento_ordinamento_modifiche();

CREATE OR REPLACE PROCEDURE spostamento_modifiche (offset_posizione INTEGER, articolo VARCHAR(255), frase_pivot INTEGER, posizione_partenza INTEGER)
LANGUAGE plpgsql
AS $procedure$
    DECLARE
        cursore_modifiche CURSOR FOR (
            SELECT m.*
            FROM frasi f
            INNER JOIN modifiche m ON f.id_frase = m.frase_modifica
            WHERE f.articolo_contenitore = articolo
            AND m.accettazione = TRUE
            AND f.id_frase != frase_pivot
            AND m.posizione >= posizione_partenza
			AND m.data_aggiornamento = (
			  	SELECT MAX(m2.data_aggiornamento)
			  	FROM modifiche m2
			  	WHERE m2.frase_modifica = m.frase_modifica AND
                accettazione = TRUE
			)

        );
    BEGIN
        DROP TRIGGER IF EXISTS creazione_modifica ON modifiche;

        FOR modifica_corrente IN cursore_modifiche LOOP
            INSERT INTO modifiche (
                data_creazione,
                posizione,
                accettazione,
                data_accettazione_frase,
                data_aggiornamento,
                autore_modifica,
                frase_modifica,
                collegamento
            ) VALUES (
                NOW(),
                modifica_corrente.posizione + offset_posizione,
                modifica_corrente.accettazione,
                modifica_corrente.data_accettazione_frase,
                NOW(),
                modifica_corrente.autore_modifica,
                modifica_corrente.frase_modifica,
                modifica_corrente.collegamento
            );
        END LOOP;

        CREATE TRIGGER creazione_modifica BEFORE
        INSERT ON modifiche FOR EACH ROW EXECUTE FUNCTION inserimento_modifica();
    END;
$procedure$;


CREATE OR REPLACE FUNCTION is_frase_cancellata (frase INTEGER)
    RETURNS BOOLEAN
    LANGUAGE plpgsql
    as $function$
        BEGIN
        RETURN EXISTS (SELECT * FROM modifiche
        WHERE frase_modifica = frase AND
        posizione = -1 AND
        accettazione = true);
        END;
    $function$;

CREATE OR REPLACE FUNCTION modifica_analoga_exists (modifica modifiche)
    RETURNS BOOLEAN
    LANGUAGE plpgsql
    as $function$
        BEGIN
        RETURN EXISTS (
                SELECT * FROM modifiche WHERE
                frase_modifica = modifica.frase_modifica AND
                posizione = modifica.posizione AND
                accettazione = TRUE AND
                data_aggiornamento = (SELECT MAX(data_aggiornamento) FROM modifiche
                WHERE frase_modifica = modifica.frase_modifica AND accettazione = TRUE)
            );
        END;
    $function$;


CREATE OR REPLACE FUNCTION is_frase_in_articolo (articolo articoli.titolo%TYPE, frase INTEGER)
    RETURNS BOOLEAN
    LANGUAGE plpgsql
    as $function$
        BEGIN
        IF get_articolo_from_frase(frase) != articolo
        THEN
            RAISE EXCEPTION 'Frase non appartenente all''articolo selezionato';
        END IF;
        RETURN is_frase_in_articolo(frase);
    END;
    $function$;

CREATE OR REPLACE FUNCTION is_frase_in_articolo (frase INTEGER)
    RETURNS BOOLEAN
    LANGUAGE plpgsql
    as $function$
        DECLARE
            articolo articoli.titolo%TYPE := get_articolo_from_frase(frase);
        BEGIN
            RETURN EXISTS (
                SELECT m.posizione, f.testo, f.id_frase, m.id_modifica
                FROM modifiche m
                INNER JOIN frasi f ON f.id_frase = m.frase_modifica
                WHERE f.articolo_contenitore = articolo
                AND m.accettazione = TRUE
                AND m.posizione > -1
                AND m.data_aggiornamento = (
                    SELECT MAX(m2.data_aggiornamento)
                    FROM modifiche m2
                    WHERE m2.frase_modifica = m.frase_modifica
                )
                AND f.id_frase = frase
            );
        END;
    $function$;

CREATE OR REPLACE PROCEDURE insert_raccordo (verso BOOLEAN,
                                            posizione_frase_da_raccordare INTEGER,
                                            riga_ordinamento ordinamento_modifiche)
    LANGUAGE plpgsql
    as $procedure$
        DECLARE verso_int INTEGER := CASE WHEN verso = TRUE THEN 1 ELSE -1 END;
        BEGIN
            INSERT INTO frasi (testo, articolo_contenitore)
                VALUES (riga_ordinamento.frase_raccordo_destra, articolo_riferimento)
                -- RETURNING id_frase INTO riga_ordinamento.new_id_frase
                ;

                INSERT INTO modifiche (
                    data_creazione,
                    posizione,
                    accettazione,
                    data_accettazione_frase,
                    data_aggiornamento,
                    autore_modifica,
                    frase_modifica,
                    collegamento
                ) VALUES (
                    NOW(),
                    posizione_frase_da_raccordare + verso_int + riga_ordinamento.offset_posizione + steps,
                    TRUE,
                    NOW(),
                    NOW(),
                    (SELECT autore_articolo FROM articolo WHERE id_articolo = articolo_riferimento),
                    riga_ordinamento.new_id_frase,
                    NULL
                );
        END;
    $procedure$;


CREATE OR REPLACE FUNCTION scaling_modifiche (posizione_originale INTEGER,
                                              data_inserimento_modifica modifiche.data_creazione %TYPE,
                                              articolo articoli.titolo %TYPE)
RETURNS INTEGER
LANGUAGE plpgsql
AS $function$
    DECLARE
        id_frase_modifica INTEGER := (SELECT frase_modifica
                             FROM modifiche
                             INNER JOIN frasi ON id_frase = frase_modifica
                             WHERE posizione = posizione_originale AND
                                   accettazione = true AND
                                   articolo_contenitore = articolo AND
                                   data_aggiornamento = (SELECT MAX (data_aggiornamento)
                                                         FROM modifiche WHERE data_aggiornamento <
                                                         data_inserimento_modifica AND
                                                         accettazione = true AND
                                                         articolo_contenitore = articolo));


        ultima_posizione_positiva INTEGER := get_ultima_posizione_positiva(id_frase_modifica, (SELECT MAX (data_aggiornamento)
                                                                           FROM modifiche WHERE posizione > -1 AND
                                                                           frase_modifica = id_frase_modifica AND
                                                                           accettazione = true));
    BEGIN
        RETURN CASE WHEN id_frase_modifica IS NULL THEN 0 ELSE ultima_posizione_positiva - posizione_originale END;
    END;
$function$;


CREATE OR REPLACE PROCEDURE controlli_modifiche_base (modifica_da_controllare modifiche)
LANGUAGE plpgsql
AS $procedure$
    BEGIN
        IF is_frase_cancellata(modifica_da_controllare.frase_modifica)
        THEN 
            RAISE EXCEPTION 'Frase già cancellata';
        END IF;

        IF modifica_da_controllare.posizione > -1 AND is_frase_in_articolo(modifica_da_controllare.frase_modifica)
        THEN
            RAISE EXCEPTION 'frase già presente';
        END IF;

        IF modifica_da_controllare.posizione = -1 AND NOT is_frase_in_articolo(modifica_da_controllare.frase_modifica)
        THEN
            RAISE EXCEPTION 'frase non cancellabile (non è presente nell''articolo)';
        END IF;

        IF modifica_analoga_exists (modifica_da_controllare.*)
        THEN
            RAISE EXCEPTION 'Modifica analoga già inserita';
        END IF;
    END;
$procedure$;

CREATE OR REPLACE FUNCTION get_articolo_from_frase (frase INTEGER) RETURNS articoli.titolo%TYPE;
LANGUAGE plpgsql
AS $function$
    BEGIN 
        RETURN (SELECT articolo_contenitore FROM frasi WHERE id_frase = frase)
    END;
$function$;


CREATE OR REPLACE FUNCTION is_modifica_revisionata (old_modifica modifiche, new_modifica modifiche)
RETURNS BOOLEAN
LANGUAGE plpgsql
AS $function$
    BEGIN
    RETURN NOT ((new_modifica.id_modifica = old_modifica.id_modifica AND
                 new_modifica.data_creazione = old_modifica.data_creazione AND
                 new_modifica.posizione = old_modifica.posizione AND
                 new_modifica.autore_modifica = old_modifica.autore_modifica AND
                 new_modifica.frase_modifica = old_modifica.frase_modifica AND
                 new_modifica.collegamento = old_modifica.collegamento) AND
                old_modifica.data_aggiornamento IS NULL);
    END;
$function$;

CREATE OR REPLACE FUNCTION get_existence_modifiche_in_attesa_in_articolo (data_creazione_minima modifiche.data_creazione%TYPE, articolo_riferimento articoli.titolo%TYPE)
RETURNS BOOLEAN
LANGUAGE plpgsql
AS $function$
    BEGIN
    RETURN EXISTS (SELECT * FROM modifiche
                  INNER JOIN frasi 
                  ON frasi.id_frase = modifiche.frase_modifica
                  WHERE data_creazione < data_creazione_minima AND
                  accettazione = false AND
                  data_aggiornamento IS NULL AND
                  frasi.articolo_contenitore = articolo_riferimento);
    END;
$function$;

CREATE OR REPLACE FUNCTION get_max_data_aggiornamento(frase INTEGER)
RETURNS modifiche.data_aggiornamento%TYPE
LANGUAGE plpgsql
AS $function$
    BEGIN
        RETURN (SELECT MAX(data_aggiornamento)
                FROM modifiche
                WHERE frase_modifica = frase AND
                accettazione = true
                );
    END;
$function$;


izione_modifica INTEGER, data_aggiornamento_minima modifiche.data_creazione%TYPE, articolo articoli.titolo%TYPE) --restituisce il numero di modifiche che hanno data_creazione != data_aggiornamento, che sono state effettuate in una posizione arbitraria e create dopo una specifica data
RETURNS INTEGER
LANGUAGE plpgsql
AS $function$
    BEGIN
    RETURN (SELECT COUNT(*) FROM modifiche
            INNER JOIN frasi
			ON frase_modifica = id_frase
            WHERE data_aggiornamento > data_aggiornamento_minima AND
            accettazione = true AND
            data_creazione != data_aggiornamento AND
            posizione = posizione_modifica AND
            articolo_contenitore = articolo
            );
    END;
$function$;


CREATE OR REPLACE FUNCTION get_ultima_posizione_positiva (frase INTEGER, max_data_aggiornamento modifiche.data_aggiornamento%TYPE)
RETURNS INTEGER
LANGUAGE plpgsql
AS $function$
    BEGIN
        RETURN (
                    SELECT posizione
                    FROM modifiche
                    WHERE frase_modifica = frase AND 
                    posizione > -1 AND 
                    accettazione = true AND
                    data_aggiornamento <= max_data_aggiornamento
                    ORDER BY data_aggiornamento DESC
                    LIMIT 1
                );
    END;
$function$;