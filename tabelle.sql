CREATE TABLE frasi (
	id_frase INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
	testo VARCHAR (255),
	articolo_contenitore VARCHAR(255) NOT NULL,

	FOREIGN KEY (articolo_contenitore) REFERENCES articoli(titolo),

	CHECK (NOT TRIM(BOTH ' ' FROM testo) = '')
);

CREATE TABLE modifiche (
	id_modifica INT GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
	data_creazione DATE NOT NULL,
	posizione INT NOT NULL,
	accettazione BOOL DEFAULT false NOT NULL,
	data_accettazione_frase DATE,
	data_aggiornamento DATE,
	autore_modifica INT NOT NULL,
	frase_modifica INT NOT NULL,
	collegamento VARCHAR(255) DEFAULT NULL,
	FOREIGN KEY (autore_modifica) REFERENCES autori(id_autore),
	FOREIGN KEY (frase_modifica) REFERENCES frasi(id_frase),
	FOREIGN KEY (collegamento) REFERENCES articoli(titolo),

	CHECK (data_creazione < data_revisione)
);


CREATE TABLE ordinamento_modifiche (
	modifica_da_ordinare INT,
	offset_posizione INT,

	PRIMARY KEY (modifica_da_ordinare),
	FOREIGN KEY (modifica_da_ordinare) REFERENCES modifiche (id_modifica)
);
