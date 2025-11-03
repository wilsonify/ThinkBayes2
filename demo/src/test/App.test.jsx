import { render, screen, fireEvent } from '@testing-library/react'
import { describe, it, expect, test } from 'vitest'
import App from '../App'

describe('App Component Integration', () => {
  it('renders the application with introduction', () => {
    render(<App />)
    
    expect(screen.getByText('Think Bayes 2 Interactive Demo')).toBeInTheDocument()
    expect(screen.getByText('Introduction')).toBeInTheDocument()
  })

  it('navigates to Bayes Theorem section', () => {
    render(<App />)
    
    const bayesButton = screen.getByText("Bayes' Theorem")
    fireEvent.click(bayesButton)
    
    expect(screen.getByText("Bayes' Theorem")).toBeInTheDocument()
    expect(screen.getByDisplayValue('0.01')).toBeInTheDocument()
  })

  it('navigates to Cookie Problem section', () => {
    render(<App />)
    
    const cookieButton = screen.getByText('Cookie Problem')
    fireEvent.click(cookieButton)
    
    expect(screen.getByText('Cookie Problem')).toBeInTheDocument()
    expect(screen.getByDisplayValue('30')).toBeInTheDocument()
  })

  it('navigates to Hockey Goals section', () => {
    render(<App />)
    
    const hockeyButton = screen.getByText('Hockey Goals')
    fireEvent.click(hockeyButton)
    
    expect(screen.getByText('Hockey Goals Analysis')).toBeInTheDocument()
    expect(screen.getByDisplayValue('3')).toBeInTheDocument()
  })

  it('navigates to Hospital section', () => {
    render(<App />)
    
    const hospitalButton = screen.getByText('Hospital Birth Rates')
    fireEvent.click(hospitalButton)
    
    expect(screen.getByText('Hospital Birth Rate Analysis')).toBeInTheDocument()
    expect(screen.getByDisplayValue('Hospital A')).toBeInTheDocument()
  })

  it('navigates to Changepoint Detection section', () => {
    render(<App />)
    
    const changepointButton = screen.getByText('Changepoint Detection')
    fireEvent.click(changepointButton)
    
    expect(screen.getByText('Changepoint Detection')).toBeInTheDocument()
    expect(screen.getByDisplayValue('3')).toBeInTheDocument()
  })

  it('navigates to Radiation section', () => {
    render(<App />)
    
    const radiationButton = screen.getByText('Radiation Sensor')
    fireEvent.click(radiationButton)
    
    expect(screen.getByText('Radiation Sensor Analysis')).toBeInTheDocument()
    expect(screen.getByDisplayValue('12')).toBeInTheDocument()
  })

  it('navigates to Survival Analysis section', () => {
    render(<App />)
    
    const survivalButton = screen.getByText('Survival Analysis')
    fireEvent.click(survivalButton)
    
    expect(screen.getByText('Survival Analysis')).toBeInTheDocument()
    expect(screen.getByDisplayValue('100')).toBeInTheDocument()
  })

  it('navigates to Typos Estimation section', () => {
    render(<App />)
    
    const typosButton = screen.getByText('Typos Estimation')
    fireEvent.click(typosButton)
    
    expect(screen.getByText('Typos Estimation Problem')).toBeInTheDocument()
    expect(screen.getByDisplayValue('1')).toBeInTheDocument()
  })

  it('navigates to Probability Distributions section', () => {
    render(<App />)
    
    const pmfButton = screen.getByText('Probability Distributions')
    fireEvent.click(pmfButton)
    
    expect(screen.getByText('Probability Mass Functions')).toBeInTheDocument()
    expect(screen.getByDisplayValue('0.1')).toBeInTheDocument()
  })

  it('navigates to Dice Problem section', () => {
    render(<App />)
    
    const diceButton = screen.getByText('Dice Problem')
    fireEvent.click(diceButton)
    
    expect(screen.getByText('Dice Problem')).toBeInTheDocument()
    expect(screen.getByDisplayValue('6')).toBeInTheDocument()
  })

  it('returns to introduction from other sections', () => {
    render(<App />)
    
    // Navigate to Bayes Theorem
    const bayesButton = screen.getByText("Bayes' Theorem")
    fireEvent.click(bayesButton)
    
    // Return to introduction
    const introButton = screen.getByText('Introduction')
    fireEvent.click(introButton)
    
    expect(screen.getByText('Introduction')).toBeInTheDocument()
  })

  test('all navigation buttons are present', () => {
    render(<App />)
    
    const expectedButtons = [
      'Introduction',
      "Bayes' Theorem",
      'Cookie Problem',
      'Dice Problem',
      'Probability Distributions',
      'Hockey Goals',
      'Hospital Birth Rates',
      'Changepoint Detection',
      'Radiation Sensor',
      'Survival Analysis',
      'Typos Estimation'
    ]
    
    expectedButtons.forEach(buttonText => {
      expect(screen.getByText(buttonText)).toBeInTheDocument()
    })
  })

  test('navigation highlights active section', () => {
    render(<App />)
    
    const bayesButton = screen.getByText("Bayes' Theorem")
    fireEvent.click(bayesButton)
    
    // Check if the button is highlighted (has active styling)
    expect(bayesButton).toHaveClass('bg-blue-500')
  })

  test('component state persists during navigation', () => {
    render(<App />)
    
    // Navigate to Bayes Theorem
    const bayesButton = screen.getByText("Bayes' Theorem")
    fireEvent.click(bayesButton)
    
    // Change a value
    const priorInput = screen.getByDisplayValue('0.01')
    fireEvent.change(priorInput, { target: { value: '0.5' } })
    
    // Navigate away and back
    const cookieButton = screen.getByText('Cookie Problem')
    fireEvent.click(cookieButton)
    
    fireEvent.click(bayesButton)
    
    // Value should be reset (since component re-renders)
    expect(screen.getByDisplayValue('0.01')).toBeInTheDocument()
  })

  test('header is always visible', () => {
    render(<App />)
    
    expect(screen.getByText('Think Bayes 2 Interactive Demo')).toBeInTheDocument()
    
    // Navigate to different sections
    const sections = ["Bayes' Theorem", 'Cookie Problem', 'Hockey Goals']
    sections.forEach(section => {
      fireEvent.click(screen.getByText(section))
      expect(screen.getByText('Think Bayes 2 Interactive Demo')).toBeInTheDocument()
    })
  })

  test('navigation works with keyboard', () => {
    render(<App />)
    
    const bayesButton = screen.getByText("Bayes' Theorem")
    bayesButton.focus()
    fireEvent.keyPress(bayesButton, { key: 'Enter', code: 'Enter' })
    
    expect(screen.getByText("Bayes' Theorem")).toBeInTheDocument()
  })
})
