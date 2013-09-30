{ 
  gStyle->SetOptStat(0);  

  TFile s500("SimplifiedSUSYMN500.0MG1000.0MSQ1000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_NJet.root"); 
  TH1F* h_s500 = (TH1F*) s500.Get("NJet");   
  h_s500 -> SetLineColor(46); 
  h_s500 -> Draw(); 
  h_s500 -> SetTitle("M_{#tilde{g}}=1000 GeV, M_{#tilde{q}}=1000 GeV");
  h_s500 -> SetXTitle("N_{jet}");
  h_s500 -> SetYTitle("number of events");

  TLatex l5(8,100,"m_{N}=500"); 
  l5.SetTextColor(38);
  TLatex l6(4,95,"m_{N}=500"); 
  l6.SetTextColor(46); 
  l5.Draw("same"); 
  l6.Draw("same"); 


  TFile u100("ADMXUDD112degenMG1000.0MQ1000.0ML50000.0MN100.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_NJet.root");
  TH1F* h_u100 = (TH1F*) u100.Get("NJet"); 
  h_u100 -> SetLineColor(30); 
  h_u100 -> Draw("same"); 
 
  TFile s100("SimplifiedSUSYMN100.0MG1000.0MSQ1000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_NJet.root"); 
  TH1F* h_s100 = (TH1F*) s100.Get("NJet");   
  h_s100 -> SetLineColor(42); 
  h_s100 -> Draw("same"); 

  TLatex l1(8,100,"m_{N}=100");
  l1.SetTextColor(30);  
  TLatex l2(4,65,"m_{N}=100"); 
  l2.SetTextColor(42);
  l1.Draw("same"); 
  l2.Draw("same"); 




  TFile u300("ADMXUDD112degenMG1000.0MQ1000.0ML50000.0MN300.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_NJet.root");
  TH1F* h_u300 = (TH1F*) u300.Get("NJet"); 
  h_u300 -> SetLineColor(35); 
  h_u300 -> Draw("same"); 
  h_u300 -> SetTitle("Missing ET, mneut=300 GeV, mgluino=msquark=1000 GeV");
 
  TFile s300("SimplifiedSUSYMN300.0MG1000.0MSQ1000.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_NJet.root"); 
  TH1F* h_s300 = (TH1F*) s300.Get("NJet");   
  h_s300 -> SetLineColor(44); 
  h_s300 -> Draw("same"); 

  TLatex l3(8,100,"m_{N}=300"); 
  l3.SetTextColor(35); 
  TLatex l4(4,80,"m_{N}=300"); 
  l4.SetTextColor(44); 
  l3.Draw("same"); 
  l4.Draw("same"); 





  TFile u500("ADMXUDD112degenMG1000.0MQ1000.0ML50000.0MN500.0_total_LHC8ATLAS_NoMatch_NoCut_AntiKT0.4_NoTau_Set_NJet.root");
  TH1F* h_u500 = (TH1F*) u500.Get("NJet"); 
  h_u500 -> SetLineColor(38); 
  h_u500 -> Draw("same"); 
  h_u500 -> SetTitle("Missing ET, mneut=500 GeV, mgluino=msquark=1000 GeV");
 
  gPad->Update();
  Double_t x1 = gPad->GetX1(); 
  Double_t x2 = gPad->GetX2();
  Double_t y1 = gPad->GetY1(); 
  Double_t y2 = gPad->GetY2(); 
  Double_t xr = x2-x1; 
  Double_t yr = y2-y1; 

  TBox b1(xr*0.7+x1,yr*0.82+y1,xr*0.72+x1,yr*0.85+y1); 
  b1.SetFillColor(42); 
  b1.Draw("same"); 

  TBox b2(xr*0.73+x1,yr*0.82+y1,xr*0.75+x1,yr*0.85+y1); 
  b2.SetFillColor(44); 
  b2.Draw("same"); 

  TBox b3(xr*0.76+x1,yr*0.82+y1,xr*0.78+x1,yr*0.85+y1); 
  b3.SetFillColor(46); 
  b3.Draw("same"); 
 
  TLatex lsim(xr*0.8+x1,yr*0.82+y1,"Sim0"); 
  lsim.SetTextColor(46); 
  lsim.Draw("same"); 

  TBox b4(xr*0.7+x1,yr*0.75+y1,xr*0.72+x1,yr*0.78+y1); 
  b4.SetFillColor(30); 
  b4.Draw("same"); 

  TBox b5(xr*0.73+x1,yr*0.75+y1,xr*0.75+x1,yr*0.78+y1); 
  b5.SetFillColor(35); 
  b5.Draw("same"); 

  TBox b6(xr*0.76+x1,yr*0.75+y1,xr*0.78+x1,yr*0.78+y1); 
  b6.SetFillColor(38); 
  b6.Draw("same"); 
  

  TLatex ludd(xr*0.8+x1,yr*0.75+y1,"UDD"); 
  ludd.SetTextColor(38); 
  ludd.Draw("same"); 


  
}
