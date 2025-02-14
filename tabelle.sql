CREATE TABLE autori (
    nickname VARCHAR(255) NOT NULL UNIQUE,
    password VARCHAR(512) NOT NULL,
    email VARCHAR(255) NOT NULL UNIQUE,
    id_autore INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,

	CHECK (NOT TRIM(BOTH ' ' FROM email) = ''),
	CHECK (NOT TRIM(BOTH ' ' FROM password) = ''),
	CHECK (NOT TRIM(BOTH ' ' FROM nickname) = '')
);

CREATE TABLE articoli (
    titolo VARCHAR(255) PRIMARY KEY,
    autore_articolo INT NOT NULL,
    data_creazione TIMESTAMP WITHOUT TIME ZONE NOT NULL,

	FOREIGN KEY (autore_articolo) REFERENCES autori(id_autore),
	CHECK (NOT TRIM(BOTH ' ' FROM titolo) = '')
);

CREATE TABLE testi_frasi (
	id_testo_frase INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
	testo VARCHAR(255) NOT NULL,
	articolo_contenitore VARCHAR(255) NOT NULL,
	autore_testo INT,

	FOREIGN KEY (articolo_contenitore) REFERENCES articoli(titolo),
	FOREIGN KEY (autore_testo) REFERENCES autori(id_autore),

	CHECK (NOT TRIM(BOTH ' ' FROM testo) = '')
);

CREATE TABLE contesti_frasi (
	id_contesto INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
	data_creazione TIMESTAMP WITHOUT TIME ZONE NOT NULL,
	posizione INT NOT NULL,
	accettazione BOOL DEFAULT false NOT NULL,
	data_revisione_testo TIMESTAMP WITHOUT TIME ZONE,
	data_aggiornamento TIMESTAMP WITHOUT TIME ZONE,
	autore_contesto INT NOT NULL,
	testo_frase INT NOT NULL,
	collegamento VARCHAR(255) DEFAULT NULL,

	FOREIGN KEY (autore_contesto) REFERENCES autori(id_autore),
	FOREIGN KEY (testo_frase) REFERENCES testi_frasi(id_testo_frase),
	FOREIGN KEY (collegamento) REFERENCES articoli(titolo),

	CHECK (data_creazione <= data_aggiornamento)
);


CREATE TABLE merge_modifiche (
	contesto_da_ordinare INT PRIMARY KEY,
	offset_posizione INT,
	visionato BOOLEAN,

	FOREIGN KEY (contesto_da_ordinare) REFERENCES contesti_frasi (id_contesto),

	CHECK (offset_posizione >= 0)
);


CREATE VIEW testi_join_contesti as 
(SELECT * FROM contesti_frasi cf
INNER JOIN testi_frasi tf ON tf.id_testo_frase = cf.testo_frase)