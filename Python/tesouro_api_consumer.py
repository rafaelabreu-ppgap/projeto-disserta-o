import tkinter as tk
from tkinter import ttk, filedialog, messagebox
import requests
import json
import csv
import pandas as pd
from datetime import datetime
import threading
import time
import urllib.parse

class TesouroAPIConsumer:
    def __init__(self, root):
        self.root = root
        self.root.title("Consumidor API Tesouro Nacional - DCA")
        self.root.geometry("800x600")
        self.root.resizable(True, True)
        
        # Variáveis de controle
        self.csv_file_path = tk.StringVar()
        self.json_data = []
        self.is_processing = False
        
        # Configurar estilo
        self.setup_style()
        
        # Criar interface
        self.create_widgets()
        
    def setup_style(self):
        """Configura o tema moderno do Tkinter"""
        style = ttk.Style()
        # Usar tema claro e moderno
        available_themes = style.theme_names()
        if 'clam' in available_themes:
            style.theme_use('clam')
        elif 'alt' in available_themes:
            style.theme_use('alt')
            
        # Configurações de cores personalizadas
        style.configure('Title.TLabel', font=('Arial', 16, 'bold'))
        style.configure('Subtitle.TLabel', font=('Arial', 10, 'italic'))
        style.configure('Success.TLabel', foreground='green')
        style.configure('Error.TLabel', foreground='red')
        style.configure('Processing.TLabel', foreground='blue')
        
    def create_widgets(self):
        """Cria todos os widgets da interface"""
        # Frame principal
        main_frame = ttk.Frame(self.root, padding="20")
        main_frame.grid(row=0, column=0, sticky=(tk.W, tk.E, tk.N, tk.S))
        
        # Configurar grid weight
        self.root.columnconfigure(0, weight=1)
        self.root.rowconfigure(0, weight=1)
        main_frame.columnconfigure(1, weight=1)
        
        # Título
        title_label = ttk.Label(main_frame, text="Consumidor API Tesouro Nacional", 
                               style='Title.TLabel')
        title_label.grid(row=0, column=0, columnspan=3, pady=(0, 10))
        
        subtitle_label = ttk.Label(main_frame, text="DCA - Anexo I-E", 
                                  style='Subtitle.TLabel')
        subtitle_label.grid(row=1, column=0, columnspan=3, pady=(0, 20))
        
        # Seção de seleção de arquivo CSV
        csv_frame = ttk.LabelFrame(main_frame, text="1. Selecionar Arquivo CSV de Controle", 
                                  padding="10")
        csv_frame.grid(row=2, column=0, columnspan=3, sticky=(tk.W, tk.E), pady=(0, 15))
        csv_frame.columnconfigure(1, weight=1)
        
        ttk.Label(csv_frame, text="Arquivo:").grid(row=0, column=0, sticky=tk.W, padx=(0, 10))
        
        self.csv_entry = ttk.Entry(csv_frame, textvariable=self.csv_file_path, 
                                  state='readonly')
        self.csv_entry.grid(row=0, column=1, sticky=(tk.W, tk.E), padx=(0, 10))
        
        self.browse_button = ttk.Button(csv_frame, text="Procurar...", 
                                       command=self.browse_csv_file)
        self.browse_button.grid(row=0, column=2, sticky=tk.W)
        
        # Informações do arquivo CSV
        self.csv_info_label = ttk.Label(csv_frame, text="Nenhum arquivo selecionado")
        self.csv_info_label.grid(row=1, column=0, columnspan=3, sticky=tk.W, pady=(5, 0))
        
        # Seção de processamento
        process_frame = ttk.LabelFrame(main_frame, text="2. Processamento dos Dados", 
                                      padding="10")
        process_frame.grid(row=3, column=0, columnspan=3, sticky=(tk.W, tk.E), pady=(0, 15))
        process_frame.columnconfigure(0, weight=1)
        
        # Botão de iniciar processamento
        self.process_button = ttk.Button(process_frame, text="Iniciar Captura de Dados", 
                                        command=self.start_processing, state='disabled')
        self.process_button.grid(row=0, column=0, pady=(0, 10))
        
        # Barra de progresso
        self.progress_var = tk.DoubleVar()
        self.progress_bar = ttk.Progressbar(process_frame, variable=self.progress_var, 
                                           maximum=100, length=400)
        self.progress_bar.grid(row=1, column=0, sticky=(tk.W, tk.E), pady=(0, 5))
        
        # Label de status
        self.status_label = ttk.Label(process_frame, text="Aguardando seleção de arquivo...")
        self.status_label.grid(row=2, column=0, sticky=tk.W)
        
        # Seção de exportação
        export_frame = ttk.LabelFrame(main_frame, text="3. Exportar Dados", padding="10")
        export_frame.grid(row=4, column=0, columnspan=3, sticky=(tk.W, tk.E), pady=(0, 15))
        
        button_frame = ttk.Frame(export_frame)
        button_frame.grid(row=0, column=0)
        
        self.export_json_button = ttk.Button(button_frame, text="Exportar para JSON", 
                                            command=self.export_to_json, state='disabled')
        self.export_json_button.grid(row=0, column=0, padx=(0, 10))
        
        self.export_csv_button = ttk.Button(button_frame, text="Exportar para CSV", 
                                           command=self.export_to_csv, state='disabled')
        self.export_csv_button.grid(row=0, column=1)
        
        # Log de atividades
        log_frame = ttk.LabelFrame(main_frame, text="Log de Atividades", padding="10")
        log_frame.grid(row=5, column=0, columnspan=3, sticky=(tk.W, tk.E, tk.N, tk.S), pady=(0, 10))
        log_frame.columnconfigure(0, weight=1)
        log_frame.rowconfigure(0, weight=1)
        main_frame.rowconfigure(5, weight=1)
        
        # Text widget com scrollbar
        log_text_frame = ttk.Frame(log_frame)
        log_text_frame.grid(row=0, column=0, sticky=(tk.W, tk.E, tk.N, tk.S))
        log_text_frame.columnconfigure(0, weight=1)
        log_text_frame.rowconfigure(0, weight=1)
        
        self.log_text = tk.Text(log_text_frame, height=10, wrap=tk.WORD)
        self.log_text.grid(row=0, column=0, sticky=(tk.W, tk.E, tk.N, tk.S))
        
        log_scrollbar = ttk.Scrollbar(log_text_frame, orient=tk.VERTICAL, command=self.log_text.yview)
        log_scrollbar.grid(row=0, column=1, sticky=(tk.N, tk.S))
        self.log_text.configure(yscrollcommand=log_scrollbar.set)
        
    def log_message(self, message):
        """Adiciona uma mensagem ao log"""
        timestamp = datetime.now().strftime("%H:%M:%S")
        self.log_text.insert(tk.END, f"[{timestamp}] {message}\n")
        self.log_text.see(tk.END)
        self.root.update_idletasks()
        
    def browse_csv_file(self):
        """Abre diálogo para selecionar arquivo CSV"""
        file_path = filedialog.askopenfilename(
            title="Selecionar arquivo CSV",
            filetypes=[("Arquivos CSV", "*.csv"), ("Todos os arquivos", "*.*")]
        )
        
        if file_path:
            self.csv_file_path.set(file_path)
            self.validate_csv_file(file_path)
            
    def validate_csv_file(self, file_path):
        """Valida o arquivo CSV selecionado"""
        try:
            # Tentar ler o arquivo CSV
            df = pd.read_csv(file_path, sep=';', encoding='utf-8')
            
            # Verificar se as colunas necessárias existem
            required_columns = ['ano', 'codigo']
            if not all(col in df.columns for col in required_columns):
                raise ValueError(f"Arquivo deve conter as colunas: {', '.join(required_columns)}")
            
            # Remover linhas vazias
            df = df.dropna()
            
            row_count = len(df)
            self.csv_info_label.config(text=f"Arquivo válido - {row_count} registros encontrados")
            self.process_button.config(state='normal')
            self.status_label.config(text="Pronto para processar dados")
            
            self.log_message(f"Arquivo CSV carregado: {file_path}")
            self.log_message(f"Encontrados {row_count} registros para processar")
            
        except Exception as e:
            error_msg = f"Erro ao validar arquivo CSV: {str(e)}"
            self.csv_info_label.config(text="Arquivo inválido")
            self.process_button.config(state='disabled')
            self.status_label.config(text="Erro na validação do arquivo")
            messagebox.showerror("Erro", error_msg)
            self.log_message(error_msg)
            
    def start_processing(self):
        """Inicia o processamento em thread separada"""
        if not self.is_processing:
            self.is_processing = True
            self.process_button.config(state='disabled')
            self.browse_button.config(state='disabled')
            thread = threading.Thread(target=self.process_data)
            thread.daemon = True
            thread.start()
            
    def process_data(self):
        """Processa os dados da API"""
        try:
            # Ler arquivo CSV
            df = pd.read_csv(self.csv_file_path.get(), sep=';', encoding='utf-8')
            df = df.dropna()
            
            total_records = len(df)
            self.json_data = []
            
            self.log_message("Iniciando captura de dados da API...")
            self.status_label.config(text="Processando dados...")
            
            # URL base da API
            base_url = "https://apidatalake.tesouro.gov.br/ords/siconfi/tt//dca"
            no_anexo = "DCA-Anexo I-E"
            
            for index, row in df.iterrows():
                try:
                    ano = str(int(row['ano']))
                    codigo = str(int(row['codigo']))
                    
                    # Construir URL com parâmetros
                    params = {
                        'an_exercicio': ano,
                        'no_anexo': no_anexo,
                        'id_ente': codigo
                    }
                    
                    self.log_message(f"Processando: Ano {ano}, Código {codigo}")
                    self.status_label.config(text=f"Processando {index+1}/{total_records}: Ano {ano}, Código {codigo}")
                    
                    # Fazer requisição à API
                    response = requests.get(base_url, params=params, timeout=30)
                    
                    if response.status_code == 200:
                        api_data = response.json()
                        
                        # Adicionar metadados aos dados
                        processed_data = {
                            'ano_exercicio': ano,
                            'id_ente': codigo,
                            'anexo': no_anexo,
                            'timestamp_captura': datetime.now().isoformat(),
                            'status_response': response.status_code,
                            'dados': api_data
                        }
                        
                        self.json_data.append(processed_data)
                        self.log_message(f"✓ Sucesso: {len(api_data.get('items', []))} registros capturados")
                        
                    else:
                        error_data = {
                            'ano_exercicio': ano,
                            'id_ente': codigo,
                            'anexo': no_anexo,
                            'timestamp_captura': datetime.now().isoformat(),
                            'status_response': response.status_code,
                            'erro': f"HTTP {response.status_code}: {response.text[:200]}"
                        }
                        self.json_data.append(error_data)
                        self.log_message(f"✗ Erro HTTP {response.status_code}: Ano {ano}, Código {codigo}")
                    
                    # Atualizar barra de progresso
                    progress = ((index + 1) / total_records) * 100
                    self.progress_var.set(progress)
                    
                    # Pequena pausa para não sobrecarregar a API
                    time.sleep(0.5)
                    
                except Exception as e:
                    error_msg = f"Erro ao processar registro {index+1}: {str(e)}"
                    self.log_message(f"✗ {error_msg}")
                    
                    error_data = {
                        'ano_exercicio': str(row.get('ano', 'N/A')),
                        'id_ente': str(row.get('codigo', 'N/A')),
                        'anexo': no_anexo,
                        'timestamp_captura': datetime.now().isoformat(),
                        'erro': str(e)
                    }
                    self.json_data.append(error_data)
                    
            # Finalizar processamento
            self.progress_var.set(100)
            self.status_label.config(text=f"Processamento concluído! {len(self.json_data)} registros processados")
            self.log_message(f"Processamento concluído com sucesso!")
            self.log_message(f"Total de registros processados: {len(self.json_data)}")
            
            # Habilitar botões de exportação
            self.export_json_button.config(state='normal')
            self.export_csv_button.config(state='normal')
            
        except Exception as e:
            error_msg = f"Erro durante o processamento: {str(e)}"
            self.log_message(f"✗ {error_msg}")
            self.status_label.config(text="Erro durante o processamento")
            messagebox.showerror("Erro", error_msg)
            
        finally:
            self.is_processing = False
            self.process_button.config(state='normal')
            self.browse_button.config(state='normal')
            
    def export_to_json(self):
        """Exporta dados para arquivo JSON"""
        if not self.json_data:
            messagebox.showwarning("Aviso", "Nenhum dado para exportar")
            return
            
        file_path = filedialog.asksaveasfilename(
            title="Salvar arquivo JSON",
            defaultextension=".json",
            filetypes=[("Arquivos JSON", "*.json"), ("Todos os arquivos", "*.*")]
        )
        
        if file_path:
            try:
                with open(file_path, 'w', encoding='utf-8') as f:
                    json.dump(self.json_data, f, indent=2, ensure_ascii=False)
                
                self.log_message(f"Dados exportados para JSON: {file_path}")
                messagebox.showinfo("Sucesso", f"Dados exportados com sucesso!\n{file_path}")
                
            except Exception as e:
                error_msg = f"Erro ao exportar JSON: {str(e)}"
                self.log_message(f"✗ {error_msg}")
                messagebox.showerror("Erro", error_msg)
                
    def export_to_csv(self):
        """Exporta dados para arquivo CSV"""
        if not self.json_data:
            messagebox.showwarning("Aviso", "Nenhum dado para exportar")
            return
            
        file_path = filedialog.asksaveasfilename(
            title="Salvar arquivo CSV",
            defaultextension=".csv",
            filetypes=[("Arquivos CSV", "*.csv"), ("Todos os arquivos", "*.*")]
        )
        
        if file_path:
            try:
                # Preparar dados para CSV
                csv_rows = []
                
                for record in self.json_data:
                    if 'dados' in record and 'items' in record['dados']:
                        # Se há dados da API, expandir cada item
                        for item in record['dados']['items']:
                            csv_row = {
                                'ano_exercicio': record['ano_exercicio'],
                                'id_ente': record['id_ente'],
                                'anexo': record['anexo'],
                                'timestamp_captura': record['timestamp_captura'],
                                'status_response': record.get('status_response', 'N/A')
                            }
                            # Adicionar campos do item da API
                            csv_row.update(item)
                            csv_rows.append(csv_row)
                    else:
                        # Se não há dados ou houve erro, criar registro de erro
                        csv_row = {
                            'ano_exercicio': record['ano_exercicio'],
                            'id_ente': record['id_ente'],
                            'anexo': record['anexo'],
                            'timestamp_captura': record['timestamp_captura'],
                            'status_response': record.get('status_response', 'N/A'),
                            'erro': record.get('erro', 'Sem dados retornados')
                        }
                        csv_rows.append(csv_row)
                
                # Escrever CSV
                if csv_rows:
                    df = pd.DataFrame(csv_rows)
                    df.to_csv(file_path, index=False, encoding='utf-8', sep=';')
                    
                    self.log_message(f"Dados exportados para CSV: {file_path}")
                    messagebox.showinfo("Sucesso", f"Dados exportados com sucesso!\n{file_path}\n{len(csv_rows)} registros")
                else:
                    messagebox.showwarning("Aviso", "Nenhum dado válido para exportar")
                    
            except Exception as e:
                error_msg = f"Erro ao exportar CSV: {str(e)}"
                self.log_message(f"✗ {error_msg}")
                messagebox.showerror("Erro", error_msg)

def main():
    root = tk.Tk()
    app = TesouroAPIConsumer(root)
    root.mainloop()

if __name__ == "__main__":
    main()